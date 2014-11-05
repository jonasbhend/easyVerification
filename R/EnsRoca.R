#' Area under the ROC curve
#' 
#' Computes the area under the ROC curve given the observations
#' 
#' @param ens n x j matrix of n probability forecasts for j categories
#' @param obs n x j matrix of occurence of n verifying observations in j categories
#' 
#' @export
EnsRoca <- function(ens, obs){
  stopifnot(is.matrix(ens), is.matrix(obs), length(obs) == length(ens))
  if (any(apply(ens, 1, sum) != 1)){
    ## convert numer of occurences to probabilities
    ens <- ens / apply(ens, 1, sum)
  }
  n.event <- apply(obs, 2, sum)
  n.total <- nrow(obs)
  ens.rank <- apply(ens, 2, rank)
  mean.rank <- apply(ens.rank * obs, 2, sum) / apply(obs, 2, sum)
  roc.area <- (mean.rank - (n.event + 1)/2) / (n.total - n.event)
  roc.area[n.event == 0] <- NA
  roc.area <- as.list(roc.area)
  names(roc.area) <- paste0('cat', seq(along=roc.area))
  return(roc.area)
}