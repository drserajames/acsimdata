# ── Internal helpers ──────────────────────────────────────────────────────────

#' Linear indices of homologous (diagonal) titre pairs
#'
#' Identifies cells where the antigen number matches the serum number under the
#' standard "AGn"/"SRn" naming convention.
#'
#' @param titre A titre matrix with rownames like "AG1", "AG2", ... and
#'   colnames like "SR1", "SR2", ...
#' @return Integer vector of linear indices into \code{titre}.
#' @noRd
.homologous_ind <- function(titre) {
  ag <- sapply(strsplit(rownames(titre), split = "AG"), "[", 2)
  sr <- sapply(strsplit(colnames(titre), split = "SR"), "[", 2)
  common <- intersect(ag, sr)
  if (length(common) == 0L) return(integer(0L))
  vapply(common, function(x) {
    rown <- which(rownames(titre) == paste0("AG", x))
    coln <- which(colnames(titre) == paste0("SR", x))
    (rown - 1L) * ncol(titre) + coln
  }, integer(1L))
}

#' Parse a character titre matrix to a numeric matrix
#'
#' Returns \code{NA} for \code{"*"} entries, the numeric value \code{X} for
#' \code{"<X"} entries, and \code{as.numeric} otherwise.
#'
#' @param titre A character (or numeric) titre matrix.
#' @return Numeric matrix of the same dimensions.
#' @noRd
.parse_titre_numeric <- function(titre) {
  tv <- as.vector(titre)
  is_star <- tv == "*"
  is_lt   <- !is_star & startsWith(tv, "<")
  nv <- suppressWarnings(as.numeric(
    ifelse(is_star, NA_character_,
           ifelse(is_lt, sub("<", "", tv, fixed = TRUE), tv))
  ))
  matrix(nv, nrow = nrow(titre), ncol = ncol(titre), dimnames = dimnames(titre))
}


# ── Exported functions ────────────────────────────────────────────────────────

#' Remove titres below a detection threshold
#'
#' Marks titres below a specified limit-of-detection as missing (\code{"*"}),
#' simulating the case where readings below the assay floor are simply not
#' reportable rather than being recorded as censored values (\code{"<X"}).
#'
#' For numeric input (e.g. \code{round_titre} from \code{\link{dist_to_hi_titre}}),
#' any value strictly less than \code{threshold} is removed.  For character
#' input (e.g. \code{lessthan_titre}), entries already encoded as \code{"<X"}
#' are removed when \code{X <= threshold}, and plain numeric string entries are
#' removed when their value is strictly less than \code{threshold}.
#'
#' @param titre A matrix of titres. Either numeric (e.g. \code{round_titre}) or
#'   character (e.g. \code{lessthan_titre}).
#' @param threshold The detection threshold in raw titre units (e.g. \code{10}
#'   for a standard HI assay floor of 1:10).
#' @param keep_homologous Whether to retain homologous titres even when they
#'   fall below \code{threshold}. Default \code{TRUE}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{full_titre}{The original titre matrix.}
#'   \item{rm_titre}{Titre matrix with removed values replaced by \code{"*"}.}
#'   \item{rm_ind}{Linear indices of newly removed cells.}
#'   \item{rm_ind_arr}{Array (row, col) indices of all \code{"*"} cells in
#'     \code{rm_titre}.}
#'   \item{params}{List of input parameters.}
#' }
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10, seed = 1)
#' ti <- dist_to_hi_titre(m$dist)
#' # Apply to numeric round_titre
#' miss_titres_threshold(ti$round_titre, threshold = 20)
#' # Apply to character lessthan_titre
#' miss_titres_threshold(ti$lessthan_titre, threshold = 20)
miss_titres_threshold <- function(titre, threshold, keep_homologous = TRUE) {

  tv <- as.vector(titre)

  if (is.numeric(titre)) {
    below <- !is.na(tv) & tv < threshold
  } else {
    is_star  <- tv == "*"
    is_lt    <- !is_star & startsWith(tv, "<")
    num_vals <- suppressWarnings(as.numeric(
      ifelse(is_star, NA_character_,
             ifelse(is_lt, sub("<", "", tv, fixed = TRUE), tv))
    ))
    # "<X": true value is < X, so mark missing when X <= threshold
    # "X" : mark missing when X < threshold
    below <- !is_star & ((is_lt & num_vals <= threshold) | (!is_lt & num_vals < threshold))
  }

  rm_ind <- which(below)

  if (keep_homologous) {
    rm_ind <- setdiff(rm_ind, .homologous_ind(titre))
  }

  rm_titre        <- titre
  rm_titre[rm_ind] <- "*"

  list(
    full_titre  = titre,
    rm_titre    = rm_titre,
    rm_ind      = rm_ind,
    rm_ind_arr  = which(rm_titre == "*", arr.ind = TRUE),
    params      = list(threshold = threshold, keep_homologous = keep_homologous)
  )
}


#' Remove titres with probability depending on titre value (informative missingness)
#'
#' Simulates missing-at-random (MAR) / missing-not-at-random (MNAR) data by
#' making each titre's probability of being removed an increasing function of
#' how low the titre is.  The probability follows a logistic curve in log2
#' titre space:
#'
#' \deqn{P(\text{missing}) = \frac{1}{1 + \exp\!\left(\text{steepness}
#'   \cdot \log_2\!\left(\frac{\text{titre}}{\text{midpoint\_titre}}\right)
#'   \right)}}
#'
#' so that \eqn{P = 0.5} at \code{midpoint_titre}, approaching 1 for very low
#' titres and 0 for very high titres.
#'
#' @param titre A character matrix of titres (e.g. \code{lessthan_titre} from
#'   \code{\link{dist_to_hi_titre}}).  Entries already coded as \code{"*"} are
#'   left unchanged; entries coded as \code{"<X"} are treated as having value
#'   \code{X} for the purpose of computing the removal probability.
#' @param midpoint_titre The raw titre value at which \eqn{P(\text{missing}) =
#'   0.5}.  Default \code{40}.
#' @param steepness Controls the sharpness of the logistic transition.  Larger
#'   values give a steeper (more threshold-like) response; smaller values give
#'   a more gradual gradient.  Default \code{1}.
#' @param keep_homologous Whether to protect homologous titres from removal.
#'   Default \code{TRUE}.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with the same structure as \code{\link{miss_titres_random}}:
#'   \code{full_titre}, \code{rm_titre}, \code{rm_ind}, \code{rm_ind_arr},
#'   \code{params}.
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10, seed = 1)
#' ti <- dist_to_hi_titre(m$dist)
#' miss_titres_informed(ti$lessthan_titre, midpoint_titre = 40, steepness = 2, seed = 42)
miss_titres_informed <- function(titre, midpoint_titre = 40, steepness = 1,
                                  keep_homologous = TRUE, seed) {
  if (missing(seed)) seed <- sample(1:1e6, 1)
  set.seed(seed)

  num_vals <- .parse_titre_numeric(titre)   # NA for "*", numeric otherwise

  # Log2-ratio relative to midpoint (NA for already-missing cells)
  log_ratio <- log2(num_vals / midpoint_titre)

  # P(missing) via logistic; positive steepness * negative log_ratio → high P for low titres
  p_miss <- 1 / (1 + exp(steepness * log_ratio))
  p_miss[is.na(num_vals)] <- 0  # already missing; exclude from sampling

  # Candidate cells: not already "*"
  tv            <- as.vector(titre)
  candidate_ind <- which(tv != "*")

  if (keep_homologous) {
    candidate_ind <- setdiff(candidate_ind, .homologous_ind(titre))
  }

  # Independent Bernoulli draw for each candidate
  drawn  <- stats::rbinom(length(candidate_ind), size = 1L, prob = p_miss[candidate_ind])
  rm_ind <- candidate_ind[drawn == 1L]

  rm_titre        <- titre
  rm_titre[rm_ind] <- "*"

  list(
    full_titre  = titre,
    rm_titre    = rm_titre,
    rm_ind      = rm_ind,
    rm_ind_arr  = which(rm_titre == "*", arr.ind = TRUE),
    params      = list(midpoint_titre = midpoint_titre, steepness = steepness,
                       keep_homologous = keep_homologous, seed = seed)
  )
}


#' Remove a rectangular block of titres
#'
#' Marks all titres within a specified antigen × serum subblock as missing
#' (\code{"*"}), simulating structured gaps such as those arising when certain
#' antigen panels are never tested against certain serum cohorts (e.g. titres
#' between geographically separate surveillance programmes).
#'
#' Both \code{antigens} and \code{sera} must be provided; specifying only one
#' would be equivalent to removing entire rows or columns (use
#' \code{\link{miss_titres_random}} with \code{proportion = 1} on a subset if
#' that is what you need).
#'
#' @param titre A character matrix of titres.
#' @param antigens Antigens to include in the block.  Either a character vector
#'   of antigen names (matching \code{rownames(titre)}) or an integer vector of
#'   row indices.
#' @param sera Sera to include in the block.  Either a character vector of serum
#'   names (matching \code{colnames(titre)}) or an integer vector of column
#'   indices.
#' @param keep_homologous Whether to protect homologous titres within the block
#'   from removal.  Default \code{TRUE}.
#'
#' @return A list with the same structure as \code{\link{miss_titres_random}}:
#'   \code{full_titre}, \code{rm_titre}, \code{rm_ind}, \code{rm_ind_arr},
#'   \code{params}.
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10, seed = 1)
#' ti <- dist_to_hi_titre(m$dist)
#' # Remove the top-left 2x3 block
#' miss_titres_block(ti$lessthan_titre, antigens = 1:2, sera = 1:3)
#' # Or specify by name
#' miss_titres_block(ti$lessthan_titre, antigens = c("AG1","AG3"), sera = c("SR2","SR4"))
miss_titres_block <- function(titre, antigens, sera, keep_homologous = TRUE) {

  n_ag <- nrow(titre)
  n_sr <- ncol(titre)

  # Resolve antigen indices
  if (is.character(antigens)) {
    ag_idx <- match(antigens, rownames(titre))
    if (any(is.na(ag_idx))) {
      stop("antigen name(s) not found in titre rownames: ",
           paste(antigens[is.na(ag_idx)], collapse = ", "))
    }
  } else {
    ag_idx <- as.integer(antigens)
    if (any(ag_idx < 1L | ag_idx > n_ag)) stop("antigen index out of range")
  }

  # Resolve serum indices
  if (is.character(sera)) {
    sr_idx <- match(sera, colnames(titre))
    if (any(is.na(sr_idx))) {
      stop("serum name(s) not found in titre colnames: ",
           paste(sera[is.na(sr_idx)], collapse = ", "))
    }
  } else {
    sr_idx <- as.integer(sera)
    if (any(sr_idx < 1L | sr_idx > n_sr)) stop("serum index out of range")
  }

  # All linear indices in the block (outer product of row × col indices)
  block_ind <- as.vector(outer(ag_idx, sr_idx, function(r, c) (r - 1L) * n_sr + c))

  # Do not re-remove already-missing cells (keep rm_ind to newly removed ones)
  already_missing <- which(as.vector(titre) == "*")
  rm_ind          <- setdiff(block_ind, already_missing)

  if (keep_homologous) {
    rm_ind <- setdiff(rm_ind, .homologous_ind(titre))
  }

  rm_titre        <- titre
  rm_titre[rm_ind] <- "*"

  list(
    full_titre  = titre,
    rm_titre    = rm_titre,
    rm_ind      = rm_ind,
    rm_ind_arr  = which(rm_titre == "*", arr.ind = TRUE),
    params      = list(antigens = antigens, sera = sera, keep_homologous = keep_homologous)
  )
}


#' Remove titres with probability depending on antigenic distance
#'
#' Simulates distance-dependent missingness: antigen–serum pairs that are far
#' apart antigenically tend to produce low (or undetectable) titres and are
#' therefore more likely to be absent from the data.  For each cell the removal
#' probability follows a logistic function of the Euclidean distance between the
#' antigen and serum coordinates:
#'
#' \deqn{P(\text{missing}) = \frac{1}{1 + \exp\!\left(-\text{steepness}
#'   \cdot (d_{ij} - \text{midpoint\_dist})\right)}}
#'
#' so that \eqn{P = 0.5} at distance \code{midpoint_dist}, rising toward 1 for
#' very distant pairs and falling toward 0 for close pairs.
#'
#' @param titre A character matrix of titres.  Rows must correspond to antigens
#'   and columns to sera, in the same order as \code{ag_coord} and
#'   \code{sr_coord} respectively.  If both the matrix and the coordinate
#'   matrices carry matching row/column names, ordering is verified automatically.
#' @param ag_coord Numeric matrix of antigen coordinates (\eqn{n_\text{ag}
#'   \times d} dimensions).  Typically \code{m$antigen_coord} from
#'   \code{\link{map_maker_random}}.
#' @param sr_coord Numeric matrix of serum coordinates (\eqn{n_\text{sr} \times
#'   d} dimensions).  Typically \code{m$sera_coord} from
#'   \code{\link{map_maker_random}}.
#' @param midpoint_dist Antigenic distance (in the same units as
#'   \code{ag_coord}/\code{sr_coord}) at which \eqn{P(\text{missing}) = 0.5}.
#'   Default \code{3}.
#' @param steepness Controls the sharpness of the logistic transition.  Default
#'   \code{1}.
#' @param keep_homologous Whether to protect homologous titres from removal.
#'   Default \code{TRUE}.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with the same structure as \code{\link{miss_titres_random}}:
#'   \code{full_titre}, \code{rm_titre}, \code{rm_ind}, \code{rm_ind_arr},
#'   \code{params}.  The list also contains \code{dist_matrix}, the
#'   \eqn{n_\text{ag} \times n_\text{sr}} matrix of pairwise Euclidean
#'   distances used for the removal probabilities.
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10, seed = 1)
#' ti <- dist_to_hi_titre(m$dist)
#' miss_titres_by_distance(
#'   ti$lessthan_titre,
#'   ag_coord     = m$antigen_coord,
#'   sr_coord     = m$sera_coord,
#'   midpoint_dist = 4,
#'   steepness    = 1,
#'   seed         = 42
#' )
miss_titres_by_distance <- function(titre, ag_coord, sr_coord,
                                     midpoint_dist = 3, steepness = 1,
                                     keep_homologous = TRUE, seed) {
  if (missing(seed)) seed <- sample(1:1e6, 1)
  set.seed(seed)

  n_ag <- nrow(titre)
  n_sr <- ncol(titre)

  # ── Validate / reorder coordinate matrices ──────────────────────────────────
  if (!is.null(rownames(ag_coord)) && !is.null(rownames(titre))) {
    m_ag <- match(rownames(titre), rownames(ag_coord))
    if (any(is.na(m_ag))) {
      stop("Not all titre rownames found in ag_coord rownames")
    }
    ag_coord <- ag_coord[m_ag, , drop = FALSE]
  } else if (nrow(ag_coord) != n_ag) {
    stop("nrow(ag_coord) (", nrow(ag_coord), ") must equal nrow(titre) (", n_ag, ")")
  }

  if (!is.null(rownames(sr_coord)) && !is.null(colnames(titre))) {
    m_sr <- match(colnames(titre), rownames(sr_coord))
    if (any(is.na(m_sr))) {
      stop("Not all titre colnames found in sr_coord rownames")
    }
    sr_coord <- sr_coord[m_sr, , drop = FALSE]
  } else if (nrow(sr_coord) != n_sr) {
    stop("nrow(sr_coord) (", nrow(sr_coord), ") must equal ncol(titre) (", n_sr, ")")
  }

  # ── Pairwise Euclidean distances ────────────────────────────────────────────
  dist_matrix <- matrix(0, nrow = n_ag, ncol = n_sr,
                        dimnames = list(rownames(titre), colnames(titre)))
  for (i in seq_len(n_ag)) {
    diffs           <- sweep(sr_coord, 2L, ag_coord[i, ], `-`)
    dist_matrix[i, ] <- sqrt(rowSums(diffs^2))
  }

  # ── Removal probabilities ───────────────────────────────────────────────────
  p_miss <- 1 / (1 + exp(-steepness * (dist_matrix - midpoint_dist)))

  # ── Sample ──────────────────────────────────────────────────────────────────
  tv            <- as.vector(titre)
  candidate_ind <- which(tv != "*")

  if (keep_homologous) {
    candidate_ind <- setdiff(candidate_ind, .homologous_ind(titre))
  }

  drawn  <- stats::rbinom(length(candidate_ind), size = 1L,
                          prob = as.vector(p_miss)[candidate_ind])
  rm_ind <- candidate_ind[drawn == 1L]

  rm_titre        <- titre
  rm_titre[rm_ind] <- "*"

  list(
    full_titre   = titre,
    rm_titre     = rm_titre,
    rm_ind       = rm_ind,
    rm_ind_arr   = which(rm_titre == "*", arr.ind = TRUE),
    dist_matrix  = dist_matrix,
    params       = list(midpoint_dist = midpoint_dist, steepness = steepness,
                        keep_homologous = keep_homologous, seed = seed)
  )
}
