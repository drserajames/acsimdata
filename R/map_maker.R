#' Make a random map
#'
#' Create random map positions and return the positions and distance matrix. Currently produces a map as a line, square, cube or hypercube
#'
#' @param n_antigens The number of antigen points
#' @param n_sera The number of serum points
#' @param range The range of the map (in all dimensions)
#' @param dimensions The number of dimensions the map
#'
#' @return
#' @export
#'
#' @examples
#'
#' map_maker_random(5,5)
map_maker_random <- function(n_antigens = 10, n_sera = 10, range = c(0,10), dimensions = 2){
  ag_coord <- matrix(runif(n_antigens*dimensions, range[1], range[2]), ncol=dimensions)
  rownames(ag_coord) <- paste0("AG", 1:n_antigens)

  sr_coord <- matrix(runif(n_sera*dimensions, range[1], range[2]), ncol=dimensions)
  rownames(sr_coord) <- paste0("SR", 1:n_antigens)

  all_coord <- rbind(ag_coord, sr_coord)

  dists <- as.matrix(dist(all_coord))

  out <- NULL
  out$coord <- all_coord
  out$dist <- dists
  return(out)
}
