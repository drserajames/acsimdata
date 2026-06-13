#' Make a random map
#'
#' Create random map positions and return the positions and distance matrix.
#'
#' @param n_antigens The number of antigen points
#' @param n_sera The number of serum points
#' @param range The maximum range of the map (in all dimensions)
#' @param dimensions The number of dimensions the map
#' @param antigen_density The density of antigen points, which can be used to indirectly specify the range. Only used when range is not specified
#' @param rdistribution A function that generates random numbers for the spatial distribution of points. The default is runif, but can be another distribution or a user generated function.
#' @param coincident TRUE if the serum points are in the same position as the antigen points, FALSE otherwise
#' @param seed Random seed
#'
#' @return list
#' @export
#'
#' @examples
#'
#' map_maker_random(5, 5, 10)
map_maker_random <- function(n_antigens, n_sera, range, dimensions = 2, antigen_density = n_antigens / range^dimensions,
                             rdistribution = stats::runif, coincident = T, seed) {
  if (missing(range)) {
    range <- (n_antigens / antigen_density)^(1 / dimensions)
  }


  if (missing(seed)) {
    seed <- sample(1:1e6, 1)
  }
  set.seed(seed)

  ag_coord <- matrix(rdistribution(n_antigens * dimensions, 0, range), ncol = dimensions)
  rownames(ag_coord) <- paste0("AG", 1:n_antigens)

  if (coincident == T) {
    sr_coord <- ag_coord[1:n_sera, ]
  } else {
    sr_coord <- matrix(rdistribution(n_sera * dimensions, 0, range), ncol = dimensions)
  }
  rownames(sr_coord) <- paste0("SR", 1:n_sera)

  all_coord <- rbind(ag_coord, sr_coord)

  dists <- as.matrix(stats::dist(all_coord))

  slim_dists <- dists[1:n_antigens, 1:n_sera+n_antigens]

  out <- list(coord = all_coord, antigen_coord = ag_coord, sera_coord = sr_coord, dist = dists, slim_dist = slim_dists, params = list(n_antigens = n_antigens, n_sera = n_sera, range = range, dimensions = dimensions, antigen_density = antigen_density, rdistribution = rdistribution, coincident = coincident, seed = seed))
  return(out)
}

#' Make a map with set coordinates for each antigen & serum
#'
#' Create random map positions for specified cluster positions and numbers.
#'
#' @param n_antigens The number of antigen points in total.
#' @param n_sera The number of serum points.
#' @param true_ag_coord A matrix with one row per antigen giving the cluster
#'   centre for that antigen.  If fewer rows than \code{n_antigens} are
#'   supplied, rows are expanded in grouped order (all cluster 1 first, then
#'   cluster 2, etc.) so that passing one row per cluster produces contiguous
#'   blocks of antigens per cluster.
#' @param true_sr_coord A matrix of cluster centres for the sera, in the same
#'   format as \code{true_ag_coord}.  Defaults to \code{true_ag_coord}.
#'   Ignored when \code{coincident} is not \code{FALSE}.
#' @param range The within-cluster scatter range (maximum offset in each
#'   dimension).
#' @param dimensions The number of map dimensions.
#' @param antigen_density Density of antigen points; used to derive
#'   \code{range} when \code{range} is not supplied.
#' @param rdistribution Distribution function for within-cluster scatter.
#'   Default \code{runif}.
#' @param coincident Controls serum coordinate generation:
#'   \describe{
#'     \item{\code{FALSE} (default)}{Sera receive independently sampled
#'       scatter around their own cluster centres (\code{true_sr_coord}).}
#'     \item{\code{TRUE}}{The first \code{n_sera} antigen positions are reused
#'       as serum positions (exact coincidence, dist = 0 for homologous pairs).
#'       \code{true_sr_coord} is ignored.}
#'     \item{integer vector of length \code{n_sera}}{The specified antigen
#'       indices are reused as serum positions.  Allows choosing which
#'       antigens become sera (e.g. one set from each cluster).
#'       \code{true_sr_coord} is ignored.}
#'   }
#'   When \code{coincident} is not \code{FALSE} the returned \code{dist} and
#'   \code{coord} matrices cover only the \code{n_antigens} antigen points
#'   (square, \eqn{n\_antigens \times n\_antigens}); \code{slim\_dist} is the
#'   \eqn{n\_antigens \times n\_sera} submatrix of those antigen-to-antigen
#'   distances at the serum column indices.
#' @param seed Random seed.
#'
#' @return A list with elements \code{coord}, \code{antigen_coord},
#'   \code{sera_coord}, \code{dist}, \code{slim_dist}, and \code{params}.
#' @export
#'
#' @examples
#' # Independent serum scatter (original behaviour)
#' map_maker_coord(5, 5, matrix(c(1,0,0,0,1,1,1,0,0,0), ncol=2, byrow=TRUE), range=10)
#'
#' # Homologous: first 3 antigens become sera
#' centres <- matrix(c(0,0, 3,0, 1.5, 2.6), ncol=2, byrow=TRUE)
#' true_ag <- centres[rep(1:3, each=4), ]
#' map_maker_coord(12, 3, true_ag, range=0.25, coincident=TRUE)
#'
#' # Homologous: specific antigen indices as sera (one per cluster)
#' map_maker_coord(12, 3, true_ag, range=0.25, coincident=c(1L, 5L, 9L))
map_maker_coord <- function(n_antigens, n_sera, true_ag_coord,
                             true_sr_coord = true_ag_coord, range,
                             dimensions = 2,
                             antigen_density = n_antigens / range^dimensions,
                             rdistribution = stats::runif,
                             coincident = FALSE,
                             seed) {

  if (missing(range)) {
    range <- (n_antigens / antigen_density)^(1 / dimensions)
  }

  if (missing(seed)) {
    seed <- sample(1:1e6, 1)
  }

  # Validate coincident
  use_coincident <- !identical(coincident, FALSE)
  if (use_coincident) {
    if (isTRUE(coincident)) {
      sera_idx <- seq_len(n_sera)
    } else {
      sera_idx <- as.integer(coincident)
      if (length(sera_idx) != n_sera) {
        stop(sprintf(
          "coincident index vector has length %d but n_sera = %d",
          length(sera_idx), n_sera
        ))
      }
      if (any(sera_idx < 1L | sera_idx > n_antigens)) {
        stop("coincident indices must be in 1:n_antigens")
      }
    }
  }

  # Expand true_ag_coord rows to n_antigens (grouped: all cluster 1 first, then cluster 2, ...)
  if (nrow(true_ag_coord) < n_antigens) {
    n_centres <- nrow(true_ag_coord)
    true_ag_coord <- true_ag_coord[
      rep(seq_len(n_centres), each = ceiling(n_antigens / n_centres))[seq_len(n_antigens)], , drop = FALSE
    ]
  }

  set.seed(seed)

  ag_coord <- true_ag_coord[seq_len(n_antigens), , drop = FALSE] +
    matrix(rdistribution(n_antigens * dimensions, 0, range), ncol = dimensions)
  rownames(ag_coord) <- paste0("AG", seq_len(n_antigens))

  if (use_coincident) {
    # Sera are exact copies of chosen antigen positions — no extra scatter
    sr_coord           <- ag_coord[sera_idx, , drop = FALSE]
    rownames(sr_coord) <- paste0("SR", seq_len(n_sera))
  } else {
    # Expand true_sr_coord rows to n_sera (grouped, matching true_ag_coord behaviour)
    if (nrow(true_sr_coord) < n_sera) {
      n_sr_centres <- nrow(true_sr_coord)
      true_sr_coord <- true_sr_coord[
        rep(seq_len(n_sr_centres), each = ceiling(n_sera / n_sr_centres))[seq_len(n_sera)], , drop = FALSE
      ]
    }
    sr_coord <- true_sr_coord[seq_len(n_sera), , drop = FALSE] +
      matrix(rdistribution(n_sera * dimensions, 0, range), ncol = dimensions)
    rownames(sr_coord) <- paste0("SR", seq_len(n_sera))
  }

  # Always return (n_antigens + n_sera) × (n_antigens + n_sera) dist matrix,
  # matching map_maker_random — coincident sera appear as duplicate rows
  all_coord  <- rbind(ag_coord, sr_coord)
  dists      <- as.matrix(stats::dist(all_coord))
  slim_dists <- dists[seq_len(n_antigens), seq_len(n_sera) + n_antigens,
                      drop = FALSE]

  list(
    coord         = all_coord,
    antigen_coord = ag_coord,
    sera_coord    = sr_coord,
    dist          = dists,
    slim_dist     = slim_dists,
    params        = list(
      n_antigens      = n_antigens,
      n_sera          = n_sera,
      true_ag_coord   = true_ag_coord,
      true_sr_coord   = if (use_coincident) NULL else true_sr_coord,
      range           = range,
      dimensions      = dimensions,
      antigen_density = antigen_density,
      rdistribution   = rdistribution,
      coincident      = coincident,
      seed            = seed
    )
  )
}
