dist_to_titre <- function(map, dists, min=10, max=2560, base=2, divisor=10){
  if(!missing(map)){
    dists <- map$dist
    n_antigens <- map$params$n_antigens
    n_sera <- map$params$n_sera
  }else{
    n_antigens <- length(grep("AG", rownames(dists)))
    n_sera <- length(grep("SR", rownames(dists)))
  }

  raw_titre <- divisor*2^dists
  round_titre <- divisor*2^round(dists)
  hi_titre <- round_titre[1:n_antigens, 1:n_sera+n_antigens]

  out <- NULL
  out$dist <- dists
  out$raw_titre <- raw_titre
  out$hi_titre <- hi_titre
  return(out)
}
