#' Add noise to antigenic distances
#'
#' Adds up to three different types of noise to the antigenic distances: per titre, per antigen and per serum. The default noise distributions are normal, but can be
#'
#' @param dists A matrix of antigenic distances
#' @param titre_noise_rdistribution Random distribution function for tire noise
#' @param titre_noise_param Arguments for the titre_noise_rdistribution function
#' @param antigen_noise_rdistribution Random distribution function for antigen noise
#' @param antigen_noise_param Arguments for the antigen_noise_rdistribution function
#' @param serum_noise_rdistribution Random distribution function for serum noise. This interacts with the max_log_titre of dist_to_hi_titre to set the highest titre for a serum.
#' @param serum_noise_param Arguments for the serum_noise_rdistribution function
#' @param seed Random seed
#'
#' @return list
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' noise_dists <- add_noise(m$dist)
add_noise <- function(dists, titre_noise_rdistribution = stats::rnorm, titre_noise_param = c(0, 1),
                      antigen_noise_rdistribution = stats::rnorm, antigen_noise_param = c(0, 1),
                      serum_noise_rdistribution = stats::runif, serum_noise_param = c(0, 1),
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

#' Add noise to a specific titre, antigen row, or serum column
#'
#' Adds noise to a single titre (specified by antigen and serum), an entire antigen row, or an entire serum column.
#' Antigens and sera can be identified by name or by integer index within the antigen×serum submatrix.
#'
#' @param dists A matrix of antigenic distances (as returned by \code{map_maker_random} or \code{map_maker_coord})
#' @param antigen Antigen name (character) or row index (integer). NULL to target all antigens for the given serum.
#' @param serum Serum name (character) or column index (integer). NULL to target all sera for the given antigen.
#' @param rdistribution Random distribution function for the noise
#' @param noise_param Arguments for the rdistribution function
#' @param seed Random seed
#'
#' @return list with dist_table, noise, noise_dist_table, and params
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' # Noise on a single titre
#' add_noise_targeted(m$dist, antigen = "AG1", serum = "SR2")
#' # Noise on all titres for antigen 1
#' add_noise_targeted(m$dist, antigen = 1)
#' # Noise on all titres for serum "SR3"
#' add_noise_targeted(m$dist, serum = "SR3")
add_noise_targeted <- function(dists, antigen = NULL, serum = NULL,
                               rdistribution = stats::rnorm, noise_param = c(0, 1),
                               seed) {
  if (is.null(antigen) && is.null(serum)) {
    stop("At least one of 'antigen' or 'serum' must be specified")
  }

  which_antigens <- grep("AG", rownames(dists))
  which_sera <- grep("SR", colnames(dists))
  dist_table <- dists[which_antigens, which_sera]
  n_antigens <- nrow(dist_table)
  n_sera <- ncol(dist_table)

  resolve_index <- function(x, names, label) {
    if (is.character(x)) {
      idx <- match(x, names)
      if (is.na(idx)) stop(label, " '", x, "' not found in distance matrix")
    } else {
      idx <- as.integer(x)
      if (idx < 1L || idx > length(names)) stop(label, " index ", idx, " out of range")
    }
    idx
  }

  ag_idx <- if (!is.null(antigen)) resolve_index(antigen, rownames(dist_table), "antigen") else NULL
  sr_idx <- if (!is.null(serum))   resolve_index(serum,   colnames(dist_table), "serum")   else NULL

  if (missing(seed)) seed <- sample(1:1e6, 1)
  set.seed(seed)

  noise_matrix <- matrix(0, nrow = n_antigens, ncol = n_sera, dimnames = dimnames(dist_table))

  if (!is.null(ag_idx) && !is.null(sr_idx)) {
    noise_matrix[ag_idx, sr_idx] <- do.call(rdistribution, as.list(c(1L, noise_param)))
  } else if (!is.null(ag_idx)) {
    noise_matrix[ag_idx, ] <- do.call(rdistribution, as.list(c(n_sera, noise_param)))
  } else {
    noise_matrix[, sr_idx] <- do.call(rdistribution, as.list(c(n_antigens, noise_param)))
  }

  list(
    dist_table = dist_table,
    noise = noise_matrix,
    noise_dist_table = dist_table + noise_matrix,
    params = list(
      antigen = antigen, serum = serum,
      rdistribution = rdistribution, noise_param = noise_param, seed = seed
    )
  )
}

add_titre_noise <- function(map, dists, titre_noise_rdistribution = stats::rnorm, titre_noise_param = c(0, 1)) {
  add_noise(map, dists,
    titre_noise_rdistribution = titre_noise_rdistribution, titre_noise_param = titre_noise_param,
    antigen_noise_param = c(0, 0),
    serum_noise_param = c(0, 0)
  )
}

add_antigen_noise <- function(map, dists, antigen_noise_rdistribution = stats::rnorm, antigen_noise_param = c(0, 1)) {
  add_noise(map, dists,
    titre_noise_param = c(0, 0),
    antigen_noise_rdistribution = antigen_noise_rdistribution, antigen_noise_param = antigen_noise_param,
    serum_noise_param = c(0, 0)
  )
}

add_serum_noise <- function(map, dists, serum_noise_rdistribution = stats::runif, serum_noise_param = c(0, 1)) {
  add_noise(map, dists,
    titre_noise_param = c(0, 0),
    antigen_noise_param = c(0, 0),
    serum_noise_rdistribution = serum_noise_rdistribution, serum_noise_param = serum_noise_param
  )
}
