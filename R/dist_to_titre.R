#' Converting antigenic distances to titres
#'
#' Takes an antigenic distance matrix and converts into values like HI titres. Antigenic distances are calculated by taking the logarithm of the HI titres, so the reverse is done here (exponentiated). So that the antigenic distances are generally whole numbers when calculated from standard HI tables, certain defaults are used.
#'
#' @param dists A matrix of antigenic distances
#' @param base The base that should be used for converting distances to titres. The default is base 2
#' @param divisor The numerator for the conversion. This is the number you divide the HI titre by when converting to an antigenic distance. The default is 10.
#' @param max_log_titre The highest logged titre possible. This can be the same for all sera, or a vector that is recycled.
#' @param min_log_titre The smallest logged titre possible. As above.
#'
#' @return list
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' dist_to_hi_titre(m$dist)
dist_to_hi_titre <- function(dists, base = 2, divisor = 10, max_log_titre = 9, min_log_titre = 0) {
  which_antigens <- grep("AG", rownames(dists))
  which_sera <- grep("SR", colnames(dists))
  n_antigens <- length(which_antigens)
  n_sera <- length(which_sera)


  dist_table <- dists[which_antigens, which_sera]
  max_log_titre_table <- matrix(max_log_titre, nrow = nrow(dist_table), ncol = ncol(dist_table))
  diff_table <- max_log_titre_table - dist_table

  raw_titre <- divisor * base^diff_table
  round_titre <- divisor * base^round(diff_table)
  lessthan_titre <- round_titre
  lessthan_titre[diff_table < min_log_titre] <- paste0("<", divisor * base^round(min_log_titre))
  lessthanhack_titre <- round_titre
  lessthanhack_titre[diff_table < min_log_titre] <- divisor * base^round(min_log_titre - 1)

  out <- list(
    dists = dists,
    raw_titre = raw_titre,
    round_titre = round_titre,
    lessthan_titre = lessthan_titre,
    lessthanhack_titre = lessthanhack_titre,
    params = list(base = base, divisor = divisor, max_log_titre = max_log_titre, min_log_titre = min_log_titre)
  )
  return(out)
}


#' Convert both true and noisy antigenic distances to HI titres
#'
#' A wrapper for dist_to_hi_titre that converts the true antigenic distances and the noisy antigenic distances, as output from add_noise
#'
#' @param noise_dists The output from add_noise
#' @param base The base that should be used for converting distances to titres. The default is base 2
#' @param divisor The numerator for the conversion. This is the number you divide the HI titre by when converting to an antigenic distance. The default is 10.
#' @param max_log_titre The highest logged titre possible. This can be the same for all sera, or a vector that is recycled.
#' @param min_log_titre The smallest logged titre possible. As above.
#'
#' @return list
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' n <- add_noise(m$dist)
#' noise_dist_to_hi_titre(n)
noise_dist_to_hi_titre <- function(noise_dists, base=2, divisor=10, max_log_titre=9, min_log_titre=0){
  true_titre <- dist_to_hi_titre(noise_dists$dist_table, base=base, divisor=divisor, max_log_titre = max_log_titre, min_log_titre = min_log_titre)
  noise_titre <- dist_to_hi_titre(noise_dists$noise_dist_table, base=base, divisor=divisor, max_log_titre = max_log_titre, min_log_titre = min_log_titre)

  out <- list(true_titre=true_titre, noise_titre=noise_titre)
}



