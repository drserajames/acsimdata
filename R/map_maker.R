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
#' Create random map positions for specified cluster positions and numbers
#'
#' @param n_antigens The number of antigen points for each cluster
#' @param n_sera The number of serum points for each cluster
#' @param true_ag_coord The coordinates of antigen points for each cluster. True coordinates need to be specified for the maximum number of dimensions.
#' @param true_sr_coord The coordinates of serum points for each cluster. True coordinates need to be specified for the maximum number of dimensions.
#' @param range The maximum range of each cluster (in all dimensions)
#' @param dimensions The number of dimensions the map
#' @param antigen_density The density of antigen points, which can be used to indirectly specify the range for a cluster. Only used when range is not specified
#' @param rdistribution A function that generates random numbers for the spatial distribution of points. The default is runif, but can be another distribition or a user generated function.
#' @param seed Random seed
#'
#' @return list
#' @export
#'
#' @examples
#'
#' map_maker_coord(5, 5, matrix(c(1,0,0,0,1,1,1,0,0,0), ncol=2, byrow=TRUE), range=10)
map_maker_coord <- function(n_antigens, n_sera, true_ag_coord, true_sr_coord=true_ag_coord, range, dimensions = 2, antigen_density = n_antigens / range^dimensions,
                             rdistribution = stats::runif, seed) {

  if (missing(range)) {
    range <- (n_antigens / antigen_density)^(1 / dimensions)
  }

  if (missing(seed)) {
    seed <- sample(1:1e6, 1)
  }

  if (nrow(true_ag_coord) != nrow(true_sr_coord)) {
    warning("true_ag_coord and true_sr_coord have different numbers of rows; recycling the shorter to match the longer.")
    max_rows <- max(nrow(true_ag_coord), nrow(true_sr_coord))
    true_ag_coord <- true_ag_coord[rep(seq_len(nrow(true_ag_coord)), length.out = max_rows), , drop = FALSE]
    true_sr_coord <- true_sr_coord[rep(seq_len(nrow(true_sr_coord)), length.out = max_rows), , drop = FALSE]
  }

  set.seed(seed)

  ag_coord <- true_ag_coord[seq_len(n_antigens), , drop = FALSE] + matrix(rdistribution(n_antigens * dimensions, 0, range), ncol = dimensions)
  rownames(ag_coord) <- paste0("AG", 1:n_antigens)

  if (sum(true_ag_coord != true_sr_coord) == 0) {
    sr_coord <- ag_coord[seq_len(n_sera), ]
  } else {
    sr_coord <- true_sr_coord[seq_len(n_sera), , drop = FALSE] + matrix(rdistribution(n_sera * dimensions, 0, range), ncol = dimensions)
  }
  rownames(sr_coord) <- paste0("SR", 1:n_sera)

  all_coord <- rbind(ag_coord, sr_coord)

  dists <- as.matrix(stats::dist(all_coord))

  slim_dists <- dists[1:n_antigens, 1:n_sera+n_antigens]

  out <- list(coord = all_coord, antigen_coord = ag_coord, sera_coord = sr_coord, dist = dists, slim_dist = slim_dists, params = list(n_antigens = n_antigens, n_sera = n_sera, true_ag_coord = true_ag_coord, true_sr_coord = true_sr_coord, range = range, dimensions = dimensions, antigen_density = antigen_density, rdistribution = rdistribution, seed = seed))
  return(out)
}
