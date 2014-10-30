#' @name EnsErrorss
#' 
#' @title
#' Compute various ensemble mean error metrics
#' 
#' @description
#' \code{EnsMess} computes the mean error, \code{EnsMaess} the mean absolute error,
#' \code{EnsMsess} the mean squared error, and \code{EnsRmsess} the square root of
#' the mean squared error (for consistency with the veri package).
#' 
#' @param ens n x k matrix of n forecasts from k ensemble members
#' @param ens.ref n x l matrix of m reference forecasts from l ensemble members
#' @param obs n verifying observations 
#' @param type specifying what error metric to compute, one of [me, mae, mse, rmse]
#' 
#' @export
EnsErrorss <- function(ens, ens.ref, obs, type){
  stopifnot(is.matrix(ens), is.matrix(ens.ref), 
            is.vector(obs), length(obs) == nrow(ens),
            length(obs) == nrow(ens.ref))
  xmask <- apply(!is.na(ens), 1, any) & !is.na(obs) & apply(!is.na(ens.ref), 1, any)
  if (all(!xmask)) {
    xout <- NA
  } else {
    xout <- 1 - EnsError(ens, obs, type) / EnsError(ens.ref, obs, type)
  }
  return(xout)
}

#' @rdname EnsErrorss
#' @export
EnsMess <- function(ens, ens.ref, obs){
  EnsErrorss(ens=ens, ens.ref=ens.ref, obs=obs, type='me')
}

#' @rdname EnsErrorss
#' @export
EnsMaess <- function(ens, ens.ref, obs){
  EnsErrorss(ens=ens, ens.ref=ens.ref, obs=obs, type='mae')
}

#' @rdname EnsErrorss
#' @export
EnsMsess <- function(ens, ens.ref, obs){
  EnsErrorss(ens=ens, ens.ref=ens.ref, obs=obs, type='mse')
}

#' @rdname EnsErrorss
#' @export
EnsRmsess <- function(ens, ens.ref, obs){
  EnsErrorss(ens=ens, ens.ref=ens.ref, obs=obs, type='rmse')
}
