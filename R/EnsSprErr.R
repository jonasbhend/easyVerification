#' Compute spread-error ratio
#' 
#' Modular function that computes the spread to error ratio (SPR) for probabilistic
#' forecasts - not unlike the functions in SpecsVerification. SPR > 1 indicates 
#' overdispersion (underconfidence), whereas SPR < indicates overconfidence in the 
#' forecasts.
#' 
#' @param ens n x k matrix of n forecasts for k ensemble members
#' @param obs vector with n verifying observations
#' 
#' @details
#' Here we define the spread-error rate as the square root of the ratio of mean
#' ensemble variance to the mean squared error of the ensemble mean with the
#' verifying observations
#' 
#' @export
EnsSprErr <- function(ens, obs){
  stopifnot(is.matrix(ens), is.vector(obs), nrow(ens) == length(obs))

  xmask <- apply(!is.na(ens), 1, any) & !is.na(obs)
  spread <- mean(apply(ens[xmask,,drop=F], 1, sd, na.rm=T)**2, na.rm=T)
  error <- mean((obs - rowMeans(ens))**2, na.rm=T)
  
  return(sqrt(spread/error))
}