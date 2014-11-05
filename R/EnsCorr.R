#' Ensemble mean correlation
#' 
#' Computes the ensemble mean correlation (Pearson) with the veryfing observations
#' 
#' @param ens n x k matrix of n forecasts from k ensemble members
#' @param obs n verifying observations
#' 
#' @examples
#' obs <- rnorm(10)
#' fcst <- array(rnorm(30), c(10,3)) + obs
#' EnsCorr(fcst, obs)
#' 
#' @export
EnsCorr <- function(ens, obs){
  stopifnot(is.matrix(ens), is.vector(obs), length(obs) == nrow(ens))
  return(cor(rowMeans(ens), obs, use='p'))
}