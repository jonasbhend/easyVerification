# veriApply.R apply verification metrics to large datasets
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

#' Apply verification metrics to large datasets
#' 
#' This wrapper applies verification metrics to arrays of forecast ensembles and
#' verifying observations. Various formats array-based formats are supported.
#' Additionally, continuous forecasts (and observations) are transformed to
#' category forecasts using user-defined absolute thresholds or percentiles of
#' the long-term climatology (see details).
#' 
#' @param verifun Name of function to compute verification metric (score, skill 
#'   score)
#' @param fcst array of forecast values (at least 2-dimensional)
#' @param obs array or vector of verifying observations
#' @param fcst.ref array of forecast values for the reference forecast (skill 
#'   scores only)
#' @param tdim index of dimension with the different forecasts
#' @param ensdim index of dimension with the different ensemble members
#' @param prob probability threshold for category forecasts (see details)
#' @param threshold absolute threshold for category forecasts (see details)
#' @param na.rm logical, should incomplete forecasts be used?
#' @param ... additional arguments passed to \code{verifun}
#'   
#' @details The probability and absolute thresholds can be supplied in various 
#'   formats. If a vector of values is supplied, the same threshold is applied 
#'   to all forecasts (e.g. lead times, spatial locations). If the thresholds 
#'   are supplied as a matrix, the number of rows has to correspond to the 
#'   number of forecasts (i.e. same length as 
#'   \code{length(fcst)/prod(dim(fcst)[c(tdim, ensdim)])}). Finally, the 
#'   thresholds can also be supplied with the dimensionality corresponding to 
#'   the \code{obs} array. In this case the dimension of the array where in 
#'   \code{obs} the forecast instances are stored holds the thresholds to be 
#'   applied to convert the continuous forecasts to category forecasts. 
#'   Consequently, this dimension can be different from the dimension in 
#'   \code{obs}.
#'   
#' @examples
#' obs <- array(rnorm(1000*30), c(1000,30))
#' fcst <- array(rnorm(1000*30*50), c(1000, 30, 50)) + 0.2*as.vector(obs)
#' f.me <- veriApply('EnsMe', fcst, obs)
#' 
#' ## find more examples and instructions in the vignette
#' \dontrun{
#' devtools::install_github("MeteoSwiss/easyVerification", build_vignettes=TRUE)
#' library('easyVerification')
#' vignette('easyVerification')
#' }
#' 
#' 
#' @keywords utilities
#' @export
#' 
veriApply <- function(verifun, fcst, obs, fcst.ref=NULL, tdim=length(dim(fcst)) - 1, ensdim=length(dim(fcst)), prob=NULL, threshold=NULL, na.rm=FALSE, ...){
  
  ## check function that is supplied
  stopifnot(exists(verifun))
  stopifnot(is.function(get(verifun)))
  
  ## check dimensions of input
  stopifnot(is.vector(obs) | is.array(obs), is.array(fcst))
  nfdims <- length(dim(fcst))
  odims <- if (is.vector(obs)) length(obs) else dim(obs)
  nodims <- length(odims)
  otdim <- min(nodims, if (ensdim < tdim) tdim - 1 else tdim)
  ## check dimensions
  stopifnot(c(ensdim, tdim) <= nfdims)
  stopifnot(odims[-otdim] == dim(fcst)[-c(ensdim, tdim)])
  ## check that only prob or threshold are supplied
  stopifnot(is.null(prob) | is.null(threshold))
  
  ## check reference forecast
  if (!is.null(fcst.ref)) stopifnot(dim(fcst)[-ensdim] == dim(fcst.ref)[-ensdim])
  
  ## make sure that forecasts (years) and ensembles are last in forecast array
  if (ensdim != nfdims | tdim != nfdims - 1){
    fcst <- aperm(fcst, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
    if (!is.null(fcst.ref)) fcst.ref <- aperm(fcst.ref, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
  }
  if (otdim != nodims){
    obs <- aperm(obs, c(setdiff(1:nodims, otdim), otdim))    
  }
  
  ## dimensions of array to compute scores
  nens <- tail(dim(fcst), 1)
  nref <- if (!is.null(fcst.ref)) tail(dim(fcst.ref), 1) else 0
  ntim <- head(tail(dim(fcst), 2), 1)
  nrest <- length(obs)/ntim
  
  ## dimensions of prob or threshold
  if (is.null(prob)){
    nprob <- 0
  } else {
    if (is.vector(prob)){
      prob <- t(prob)[rep(1,nrest),]
    } else if (length(dim(prob)) == nodims){
      prob <- aperm(prob, c(setdiff(1:nodims, otdim), otdim))
    }
    stopifnot(length(prob)%%nrest == 0)
    prob <- array(prob, c(nrest, length(prob)/nrest))
    nprob <- ncol(prob)
  }
  if (is.null(threshold)){
    nthresh <- 0
  } else {
    if (is.vector(threshold)){
      threshold <- t(threshold)[rep(1,nrest),]
    } else if (length(dim(threshold)) == nodims){
      threshold <- aperm(threshold, c(setdiff(1:nodims, otdim), otdim))
    }
    stopifnot(length(threshold)%%nrest == 0)
    threshold <- array(threshold, c(nrest, length(threshold)/nrest))
    nthresh <- ncol(threshold)
  }
  
  ## figure out how many 3rd dimensions are needed to write prob/thresh
  nconv <- ceiling((nprob + nthresh)/ntim)
  
  ## fill in xall with additional obs
  xall <- array(c(fcst, fcst.ref, obs, rep(obs*NA, nconv)), c(nrest, ntim, nens+nref+1+nconv))
  if (nconv > 0){
    probthresh <- rbind(prob, threshold)
    for (j in 1:nconv){
      ind <- seq((j - 1)*ntim + 1, min(ntim*j, ncol(probthresh))) - (j-1)*ntim
      xall[,ind,nens+nref+1+j] <- probthresh[,ind]
    }
  }
  ## mask missing values
  if (na.rm) {
    xmask <- apply(apply(!is.na(xall[,,1:(nens+nref+1),drop=F]), 1:2, all), 1, any)
  } else {
    xmask <- apply(!is.na(xall[,,1:(nens+nref+1),drop=F]), 1, all)
  }
  ## check whether there are complete forecast/observation pairs at all
  stopifnot(any(xmask))
 
  ## indices for re-expansion of output
  maskexpand <- rep(NA, length(xmask))
  maskexpand[xmask] <- 1:sum(xmask)  
  
  ## run the workhorse
  Tmatrix <- function(x) if (is.matrix(x)) t(x) else as.matrix(x)
    
  out <- Tmatrix(apply(xall[xmask,,,drop=F], 
                       MARGIN=1, 
                       FUN=veriUnwrap, 
                       verifun=verifun, 
                       nind=c(nens=nens, nref=nref, nobs=1, nprob=nprob, nthresh=nthresh),
                       ...))
  
  ## reformat the output by converting to list
  if (is.list(out)){
    lnames <- names(out[[1]])
    olist <- list()
    for (ln in lnames) olist[[ln]] <- sapply(out, function(x) x[[ln]])
  } else {
    olist <- list(out)
  }

  ## reexpand the masked values
  olist <- lapply(olist, function(x) as.matrix(x)[maskexpand,])
  
  ## rearrange output to original dimensions
  out <- lapply(olist, function(x){
    if (length(x) == length(fcst)){
      ## repermute the output
      fperm <- 1:nfdims
      fperm[c(tdim, ensdim)] <- nfdims - 1:0
      xout <- aperm(array(x, dim(fcst)), fperm)
    } else if (nodims > 1){
      if (length(x) == length(obs)){
        operm <- 1:nodims
        operm[otdim] <- nodims
        xout <- if (nodims == 1) c(x) else aperm(array(x, odims), operm)
      } else if (length(x) == prod(odims[-otdim])) {
        xout <- array(x, odims[-otdim])
      } 
    } else {
      xout <- x
    }
    return(xout)
  })
  
  ## if output list is of length one, output object within list
  if (length(out) == 1) out <- out[[1]]
  
  return(out)
  
}