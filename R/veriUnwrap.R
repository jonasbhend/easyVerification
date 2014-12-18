# veriUnwrap.R unwrap arguments to hand over to verification functions
#
#     Copyright (C) 2014 MeteoSwiss
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' Unwrap arguments and hand over to function
#' 
#' decomposes input argument into forecast and verifying observations
#' and hands these over to the function provided
#' 
#' @param x n x k + 1 matrix with n forecasts of k ensemble members
#' plus the verifying observations
#' @param verifun character string with function name to be executed
#' @param nens number of ensemble members in forecast (see details)
#' @param prob probability threshold for conversion of forecasts to 
#' probability forecasts as in \code{\link{convert2prob}}
#' @param threshold absolute threshold for conversion of forecasts to 
#' probability forecasts as in \code{\link{convert2prob}}
#' @param ... additional arguments passed on to \code{verifun}
#' 
#' @details
#' Only forecasts with non-missing observation and complete ensembles are
#' computed. All other forecasts are set to missing. For aggregate metrics
#' (e.g. skill scores) the metric is computed over non-missing observation/forecast
#' pairs only.
#' 
#' For computation of skill scores, reference forecasts can be provided. That is, the
#' first \code{nens} columns of \code{x} contain the forecasts, the \code{(nens + 1):(ncol(x) - 1)}
#' following columns contain the reference forecast, and the final column contains the observations.
#' If no reference forecast is provided (i.e. \code{ncol(x) == nens + 1}), a climatological
#' forecast is constructed from the \code{n} verifying observations.
#' 
veriUnwrap <- function(x, verifun, nens=ncol(x) - 1, prob=NULL, threshold=NULL, ...){
  nn <- ncol(x)
  stopifnot(nn >= nens + 1)
  vfun <- get(verifun)
  ## mask missing values
  xmask <- apply(!is.na(x), 1, all)
  x <- x[xmask,]
  ## check whether this is a skill score or a score
  is.skill <- substr(verifun, nchar(verifun) - 1, nchar(verifun)) == 'ss'
  if (is.skill){
    if (nn > nens + 1){
      xref <- x[,-c(1:nens, nn)]
    } else {
      xref <- t(array(x[,nn], c(nrow(x), nrow(x))))      
    }
    out <- vfun(convert2prob(x[,1:nens], prob=prob, threshold=threshold),
                convert2prob(xref, prob=prob, threshold=threshold),
                convert2prob(x[,nn], prob=prob, threshold=threshold), ...)
  } else {
    stopifnot(nn == nens + 1)
    out <- vfun(convert2prob(x[,1:nens], prob=prob, threshold=threshold),
                convert2prob(x[,nn], prob=prob, threshold=threshold), ...)    
  }

  ## check whether output has to be expanded with NA
  is.expand <- !all(xmask) & (length(out) == sum(xmask) | is.list(out))
  if (is.list(out)) is.expand <- any(sapply(out, length) == sum(xmask))
  if (is.expand){
    maskexpand <- rep(NA, length(xmask))
    maskexpand[xmask] <- 1:sum(xmask)
    if (is.list(out)){
      out <- lapply(out, function(x){
        if (length(x) == sum(xmask)){
          x <- x[maskexpand]
        }
        return(x)
      })  
    } else if (length(out) == sum(xmask)) {
      out <- out[maskexpand]
    }    
  }
  
  return(out)
}