#' Remove titres to create random missing data
#'
#' Generates a titre table with missing data
#'
#' @param titre A matrix of titres
#' @param proportion The proportion of titres to be removed
#' @param keep_homologous Whether to retain homologous titres
#'
#' @return list
#' @export
#'
#' @examples
#' m <- map_maker_random(5, 5, 10)
#' ti <- dist_to_hi_titre(m$dist)
#' miss_titres_random(ti$lessthan_titre, 0.2)
miss_titres_random <- function(titre, proportion, keep_homologous=T){
  n_titres <- length(titre)
  rm_number <- round(proportion*n_titres)
  relevant_ind <- 1:n_titres

  if (keep_homologous==T){
    ag <- sapply(strsplit(rownames(titre), split="AG"), "[",2)
    sr <- sapply(strsplit(colnames(titre), split="SR"), "[",2)
    common <- intersect(ag, sr)
    diag_titre <- rep(NA, length(common))
    for (i in 1:length(common)){
      rown <- which(rownames(titre)==paste0("AG", common[i]))
      coln <- which(colnames(titre)==paste0("SR", common[i]))
      diag_titre[i] <- (rown-1)*ncol(titre)+coln
}
relevant_ind <- relevant_ind[-diag_titre]
  }

  rm_ind <- sample(relevant_ind, rm_number)
  rm_titre <- titre
  rm_titre[rm_ind] <- "*"

  out <- list(full_titre = titre, rm_titre = rm_titre, rm_ind = rm_ind, rm_ind_arr = which(rm_titre=="*", arr.ind=T),
              params = list(proportion=proportion, keep_homologous=keep_homologous))
  return(out)
}


