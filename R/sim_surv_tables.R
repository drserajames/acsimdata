#' Simulate multi-period surveillance HI data
#'
#' Generates a series of HI titre tables mimicking the structure of
#' influenza surveillance data, where each table contains a partially
#' overlapping sliding window of antigens and sera. The merged table has a
#' banded structure of missing values because antigens and sera from
#' non-overlapping time periods are never tested together.
#'
#' Antigens and sera are organised into a sliding window: each new table
#' drops the \code{n_antigens_per_table - n_ag_overlap} oldest antigens and
#' adds the same number of new ones. Spatial positions drift along the first
#' dimension by \code{ag_drift} units per table period, reflecting antigenic
#' evolution over time.
#'
#' @param n_tables Number of sequential tables (time periods).
#' @param n_antigens_per_table Total number of antigens in each table.
#' @param n_sera_per_table Total number of sera in each table.
#' @param n_ag_overlap Number of antigens shared between consecutive tables.
#'   Must be less than \code{n_antigens_per_table}.
#' @param n_sr_overlap Number of sera shared between consecutive tables.
#'   Must be less than \code{n_sera_per_table}.
#' @param ag_drift Antigenic distance drifted per table period, applied along
#'   the first map dimension. Controls how far apart successive antigen clusters
#'   are.
#' @param range Maximum within-cluster spatial scatter of individual points
#'   around their cluster centre (upper bound passed to \code{rdistribution}).
#' @param dimensions Number of map dimensions.
#' @param base Base for titre conversion (default 2, standard for HI assays).
#' @param divisor Divisor for titre conversion (default 10).
#' @param max_log_titre Highest log titre possible, applied uniformly to all
#'   sera.
#' @param min_log_titre Lowest detectable log titre; measurements below this
#'   become threshold titres (e.g. \code{"<10"}).
#' @param rdistribution Random distribution function for within-cluster scatter.
#'   Default is \code{stats::runif}.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with elements:
#' \describe{
#'   \item{merged_titre_table}{Full merged HI table across all time periods.
#'     Untested antigen-serum combinations are encoded as \code{"*"}.
#'     Where a pair appears in multiple tables due to overlap, the value from
#'     the most recent table is used.}
#'   \item{ag_coord}{Matrix of true antigen coordinates
#'     (total antigens x dimensions).}
#'   \item{sr_coord}{Matrix of true serum coordinates
#'     (total sera x dimensions).}
#'   \item{ag_table_membership}{Named list: for each table, a character vector
#'     of the antigen names it contains.}
#'   \item{sr_table_membership}{Named list: for each table, a character vector
#'     of the serum names it contains.}
#'   \item{tables}{Named list of per-table results, each being the output of
#'     \code{\link{dist_to_hi_titre}}.}
#'   \item{params}{List of input parameters.}
#' }
#' @export
#'
#' @examples
#' sim_surv_tables(
#'   n_tables = 5, n_antigens_per_table = 8, n_sera_per_table = 6,
#'   n_ag_overlap = 3, n_sr_overlap = 2, ag_drift = 3, range = 1, seed = 1
#' )
sim_surv_tables <- function(n_tables, n_antigens_per_table, n_sera_per_table,
                            n_ag_overlap, n_sr_overlap,
                            ag_drift = 3, range = 1, dimensions = 2,
                            base = 2, divisor = 10,
                            max_log_titre = 9, min_log_titre = 0,
                            rdistribution = stats::runif,
                            seed) {

  if (n_ag_overlap >= n_antigens_per_table) {
    stop("n_ag_overlap must be less than n_antigens_per_table")
  }
  if (n_sr_overlap >= n_sera_per_table) {
    stop("n_sr_overlap must be less than n_sera_per_table")
  }

  n_new_ag <- n_antigens_per_table - n_ag_overlap
  n_new_sr <- n_sera_per_table - n_sr_overlap

  total_ag <- n_antigens_per_table + (n_tables - 1L) * n_new_ag
  total_sr <- n_sera_per_table + (n_tables - 1L) * n_new_sr

  if (missing(seed)) seed <- sample(1:1e6, 1)
  set.seed(seed)

  # Introduction table for each antigen and serum.
  # Antigens (and sera) are numbered in order of introduction. The first batch
  # (indices 1..n_per_table) are all introduced in table 1. Thereafter,
  # n_new new ones are introduced per table.
  ag_intro <- pmax(1L, ceiling((seq_len(total_ag) - n_antigens_per_table) / n_new_ag) + 1L)
  sr_intro <- pmax(1L, ceiling((seq_len(total_sr) - n_sera_per_table) / n_new_sr) + 1L)

  # Coordinates: uniform scatter within [0, range] added to a cluster centre
  # that drifts by ag_drift along the first dimension per table period.
  ag_coord <- matrix(rdistribution(total_ag * dimensions, 0, range),
                     nrow = total_ag, ncol = dimensions)
  ag_coord[, 1] <- ag_coord[, 1] + (ag_intro - 1L) * ag_drift
  rownames(ag_coord) <- paste0("AG", seq_len(total_ag))

  sr_coord <- matrix(rdistribution(total_sr * dimensions, 0, range),
                     nrow = total_sr, ncol = dimensions)
  sr_coord[, 1] <- sr_coord[, 1] + (sr_intro - 1L) * ag_drift
  rownames(sr_coord) <- paste0("SR", seq_len(total_sr))

  # Sliding window membership: table t contains points with index in
  # [(t-1)*n_new + 1,  (t-1)*n_new + n_per_table].
  ag_in_table <- lapply(seq_len(n_tables), function(t) {
    paste0("AG", seq((t - 1L) * n_new_ag + 1L, (t - 1L) * n_new_ag + n_antigens_per_table))
  })
  sr_in_table <- lapply(seq_len(n_tables), function(t) {
    paste0("SR", seq((t - 1L) * n_new_sr + 1L, (t - 1L) * n_new_sr + n_sera_per_table))
  })
  names(ag_in_table) <- names(sr_in_table) <- paste0("table", seq_len(n_tables))

  # Full pairwise distance matrix for all points.
  all_coord <- rbind(ag_coord, sr_coord)
  full_dist <- as.matrix(stats::dist(all_coord))

  # Per-table titre results.
  tables <- lapply(seq_len(n_tables), function(t) {
    sub_dist <- full_dist[ag_in_table[[t]], sr_in_table[[t]], drop = FALSE]
    dist_to_hi_titre(sub_dist,
                     base = base, divisor = divisor,
                     max_log_titre = max_log_titre, min_log_titre = min_log_titre)
  })
  names(tables) <- paste0("table", seq_len(n_tables))

  # Build the merged titre table.  Initialise with "*" (untested), then fill
  # in each table in chronological order so that overlap cells take the value
  # from the most recent table.
  all_ag_names <- paste0("AG", seq_len(total_ag))
  all_sr_names <- paste0("SR", seq_len(total_sr))
  merged <- matrix("*", nrow = total_ag, ncol = total_sr,
                   dimnames = list(all_ag_names, all_sr_names))

  for (t in seq_len(n_tables)) {
    ags <- ag_in_table[[t]]
    srs <- sr_in_table[[t]]
    merged[ags, srs] <- tables[[t]]$lessthan_titre
  }

  list(
    merged_titre_table  = merged,
    ag_coord            = ag_coord,
    sr_coord            = sr_coord,
    ag_table_membership = ag_in_table,
    sr_table_membership = sr_in_table,
    tables              = tables,
    params = list(
      n_tables             = n_tables,
      n_antigens_per_table = n_antigens_per_table,
      n_sera_per_table     = n_sera_per_table,
      n_ag_overlap         = n_ag_overlap,
      n_sr_overlap         = n_sr_overlap,
      n_new_ag             = n_new_ag,
      n_new_sr             = n_new_sr,
      ag_drift             = ag_drift,
      range                = range,
      dimensions           = dimensions,
      base                 = base,
      divisor              = divisor,
      max_log_titre        = max_log_titre,
      min_log_titre        = min_log_titre,
      rdistribution        = rdistribution,
      seed                 = seed
    )
  )
}
