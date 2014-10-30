#' @name EnsError
#' 
#' @title
#' Compute various ensemble mean error metrics
#' 
#' @description
#' \code{EnsMe} computes the mean error, \code{EnsMae} the mean absolute error,
#' \code{EnsMse} the mean squared error, and \code{EnsRmse} the square root of
#' the mean squared error (for consistency with the veri package).
#' 
#' @param ens n x k matrix of n forecasts from k ensemble members
#' @param obs n verifying observations 
#' @param type specifying what error metric to compute, one of [me, mae, mse, rmse]
#' 
#' @export
EnsError <- function(ens, obs, type){
  stopifnot(is.matrix(ens), is.vector(obs), length(obs) == nrow(ens))
  xmask <- apply(!is.na(ens), 1, any) & !is.na(obs)
  if (all(!xmask)) {
    xout <- NA
  } else {
    error <- rowMeans(ens) - obs    
    if (type == 'me'){
      xout <- mean(error, na.rm=T)
    } else if (type == 'mae') {
      xout <- mean(abs(error), na.rm=T)
    } else if (type == 'mse') {
      xout <- mean(error**2, na.rm=T)
    } else if (type == 'rmse') {
      xout <- sqrt(mean(error**2, na.rm=T))
    }
  }
  return(xout)
}

#' @rdname EnsError
#' @export
EnsMe <- function(ens, obs){
  EnsError(ens=ens, obs=obs, type='me')
}

#' @rdname EnsError
#' @export
EnsMae <- function(ens, obs){
  EnsError(ens=ens, obs=obs, type='mae')
}

#' @rdname EnsError
#' @export
EnsMse <- function(ens, obs){
  EnsError(ens=ens, obs=obs, type='mse')
}

#' @rdname EnsError
#' @export
EnsRmse <- function(ens, obs){
  EnsError(ens=ens, obs=obs, type='rmse')
}
