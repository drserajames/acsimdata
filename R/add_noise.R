#' Add noise to antigenic distances
#'
#' Adds up to three different types of noise to the antigenic distances: per titre, per antigen and per serum. The default noise distributions are normal, but can be
#'
#' @param dists A matrix of antigenic distances
#' @param titre_noise_rdistribution Random distribution function for tire noise
#' @param titre_noise_param Arguments for the titre_noise_rdistribution function
#' @param antigen_noise_rdistribution Random distribution function for antigen noise
#' @param antigen_noise_param Arguments for the antigen_noise_rdistribution function
#' @param serum_noise_rdistribution Random distribution function for serum noise
#' @param serum_noise_param Arguments for the serum_noise_rdistribution function
#' @param seed Random seed
#'
#' @return list
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' noise_dists <- add_noise(m$dist)
add_noise <- function(dists, titre_noise_rdistribution = stats::rnorm, titre_noise_param = c(0, 0.5),
                      antigen_noise_rdistribution = stats::rnorm, antigen_noise_param = c(0, 0.5),
                      serum_noise_rdistribution = stats::rnorm, serum_noise_param = c(0, 0.5),
                      seed) {
  which_antigens <- grep("AG", rownames(dists))
  which_sera <- grep("SR", colnames(dists))
  n_antigens <- length(which_antigens)
  n_sera <- length(which_sera)


  if (missing(seed)) {
    seed <- sample(1:1e6, 1)
  }
  set.seed(seed)

  dist_table <- dists[which_antigens, which_sera]

  titre_noise <- matrix(do.call("titre_noise_rdistribution", as.list(c(n_antigens * n_sera, titre_noise_param))), nrow = n_antigens, ncol = n_sera)
  antigen_noise <- matrix(do.call("antigen_noise_rdistribution", as.list(c(n_antigens, antigen_noise_param))), nrow = n_antigens, ncol = n_sera)
  serum_noise <- matrix(do.call("serum_noise_rdistribution", as.list(c(n_sera, serum_noise_param))), nrow = n_antigens, ncol = n_sera, byrow = T)
  total_noise <- titre_noise + antigen_noise + serum_noise

  noise_dist_table <- dist_table + total_noise

  out <- list(
    dist_table = dist_table, titre_noise = titre_noise, antigen_noise = antigen_noise, serum_noise = serum_noise, total_noise = total_noise, noise_dist_table = noise_dist_table,
    params = list(
      titre_noise_rdistribution = titre_noise_rdistribution, titre_noise_param = titre_noise_param,
      antigen_noise_rdistribution = antigen_noise_rdistribution, antigen_noise_param = antigen_noise_param,
      serum_noise_rdistribution = serum_noise_rdistribution, serum_noise_param = serum_noise_param,
      seed = seed
    )
  )
  return(out)
}

add_titre_noise <- function(map, dists, titre_noise_rdistribution = stats::rnorm, titre_noise_param = c(0, 0.5)) {
  add_noise(map, dists,
    titre_noise_rdistribution = titre_noise_rdistribution, titre_noise_param = titre_noise_param,
    antigen_noise_param = c(0, 0),
    serum_noise_param = c(0, 0)
  )
}

add_antigen_noise <- function(map, dists, antigen_noise_rdistribution = stats::rnorm, antigen_noise_param = c(0, 0.5)) {
  add_noise(map, dists,
    titre_noise_param = c(0, 0),
    antigen_noise_rdistribution = antigen_noise_rdistribution, antigen_noise_param = antigen_noise_param,
    serum_noise_param = c(0, 0)
  )
}

add_serum_noise <- function(map, dists, serum_noise_rdistribution = stats::rnorm, serum_noise_param = c(0, 0.5)) {
  add_noise(map, dists,
    titre_noise_param = c(0, 0),
    antigen_noise_param = c(0, 0),
    serum_noise_rdistribution = serum_noise_rdistribution, serum_noise_param = serum_noise_param
  )
}
