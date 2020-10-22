#' Make a random map
#'
#' Create random map positions and return the positions and distance matrix. Currently produces a map as a line, square, cube or hypercube.
#'
#' @param n_antigens The number of antigen points
#' @param n_sera The number of serum points
#' @param range The maximum range of the map (in all dimensions)
#' @param dimensions The number of dimensions the map
#' @param antigen_density The density of antigen points, which can be used to indirectly specify the range. Only used when range is not specified
#' @param rdistribution A function that generates random numbers for the spatial distribution of points. The default is runif and a reasonable option is rnorm or a user generated function.
#' @param coincident TRUE if the serum points are in the same position as the antigen points, FALSE otherwise
#'
#' @return
#' @export
#'
#' @examples
#'
#' map_maker_random(5, 5, 10)
map_maker_random <- function(n_antigens, n_sera, range = NA, dimensions = 2, antigen_density = n_antigens/range^dimensions,
                             rdistribution = runif, coincident=T){
  if(is.na(range)){
    range <- (n_antigens/density)^(1/dimensions)
  }

  ag_coord <- matrix(rdistribution(n_antigens*dimensions, 0, range), ncol=dimensions)
  rownames(ag_coord) <- paste0("AG", 1:n_antigens)

  if (coincident == T){
    sr_coord <- ag_coord[1:n_sera,]
  }else{
  sr_coord <- matrix(rdistribution(n_sera*dimensions, 0, range), ncol=dimensions)
  }
  rownames(sr_coord) <- paste0("SR", 1:n_sera)

  all_coord <- rbind(ag_coord, sr_coord)

  dists <- as.matrix(dist(all_coord))

  out <- NULL
  out$coord <- all_coord
  out$dist <- dists
  return(out)
}
