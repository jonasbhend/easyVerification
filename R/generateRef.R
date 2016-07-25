# generateRef Generate Probabilistic Climatological Ensemble Forecast from Observations
#
#     Copyright (C) 2016 MeteoSwiss
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

#' @name generateRef
#' @aliases indRef
#' 
#' @title 
#' Generate Probabilistic Climatological Ensemble Forecast from Observations
#' 
#' @description
#' To generate reference ensemble forecasts for forecast evaluation based on
#' the available observations, \code{indRef} implements the out-of-sample or
#' in-sample protocol to be used and \code{generateRef} produces the corresponding
#' ensemble forecast given the actual observations.
#' 
#' @param nfcst number of forecast instances to be produce
#' @param type type of out-of-sample protocol to be applied (see below)
#' @param indices Subset of the observations / forecast times to be used for
#'   reference forecasts
#' @param blocklength for cross-validation
#' 
#' @return 
#'   \item{ind}{A list of indices to be used for each forecast from \code{1} to \code{nfcst}}
#'  
#' @section Cross-validation: Leave-one-out and leave-n-out cross-validation
#'   reference forecasts can be produced by setting \code{type = "crossval"}.
#'   By default, the blocklength is set to \code{1}, but moving blocks of
#'   length \code{n} can be specified by setting \code{blocklength = n}. 
#'   
#' @section Forward: Correspondingly, reference forecasts that are only based on past (future) 
#'   observations can be produced using \code{type = "forward"}. For this, the 
#'   first half of the reference forecasts only uses future information, i.e. 
#'   observations \code{2:n} for forecast \code{1}, \code{3:n} for \code{2} and 
#'   so forth. The second half of the reference forecasts use only past 
#'   observations, i.e. observations \code{1:(n-1)} for forecast \code{n}, 
#'   \code{1:(n-2)} for \code{n-1}, etc.
#'   
#'   
#'   
#'   \code{ref.opts} specifies the 
#'   set-up of the climatological reference forecast for skill scores if no 
#'   explicit reference forecast is provided. \code{ref.opts} can be either set 
#'   to \code{NULL}, that is all available observations are used as equiprobable
#'   members of a reference forecast, or a list with \code{n} elements 
#'   containing the indices of the observations to be used to construct the 
#'   reference forecast for forecast \code{n} can be provided. The indices 
#'   provided have to be non-missing and in the range of \code{1} to \code{n} of
#'   the verifying observations.
#'   
#'   Correspondingly, reference forecasts that are only based on past (future) 
#'   observations can be produced using \code{ref.opts="forward"} or 
#'   \code{ref.opts = list(forward=TRUE)}. For the \code{forward} method, the 
#'   first half of the reference forecasts only uses future information, i.e. 
#'   observations \code{2:n} for forecast \code{1}, \code{3:n} for \code{2} and 
#'   so forth. The second half of the reference forecasts use only past 
#'   observations, i.e. observations \code{1:(n-1)} for forecast \code{n}, 
#'   \code{1:(n-2)} for \code{n-1}, etc.
#'   
#'   In combination with the above, a subset of the observations can be 
#'   specified for use as reference forecasts by providing the explicit indices 
#'   of the observations to be used via \code{ref.opts = list(..., 
#'   indices=1:k)}. In combination with the \code{forward} method, all
#'   observations in \code{ref.opts$indices} will be used to construct the
#'   reference forecast for forecasts not included in \code{ref.opts$indices}.
#'   In combination with the \code{crossval} method with \code{blocklength > 1},
#'   observations are used for the reference forecasts that are included in
#'   \code{ref.opts$indices}, but not in the block around the forecast index,
#'   where this block is defined on all indices and not on the indices supplied
#'   by \code{ref.opts$indices}.
#' 
#' @keywords utilities
#' @export
#' 
indRef <- function(nfcst, type=c('none', 'forward', 'crossval', 'block'), 
                   indices=1:nfcst, blocklength=1){
  
  ## check type of out-of-sample climatological reference generation
  type <- match.arg(type)
  stopifnot(nfcst %% 1 == 0)
  
  if (type == 'none'){
    ind <- lapply(1:nfcst, function(x) indices)
  } else if (type == 'forward') {
    stopifnot(length(indices) > 1)
    ind <- lapply(1:nfcst, function(x) indices) 
    iinds <- indices[indices %in% 1:nfcst]
    ind[iinds] <- lapply(seq(along=iinds), function(x) {
      indices[seq(ifelse(x > (length(indices) %/% 2), 1, x+1), 
                  ifelse( x > (length(indices) %/% 2), x-1, length(indices)))]
    })
  } else {
    stopifnot(blocklength < length(indices))
    if (type == 'crossval') {
      ind <- lapply(1:nfcst, function(x) {
        mini <- min(nfcst - blocklength + 1, max(1, x - blocklength %/% 2))
        maxi <- max(blocklength, min(nfcst, x + (blocklength - 1) %/% 2))
        return(setdiff(indices, mini:maxi))
        })
    } else if (type == 'block'){
      ind <- lapply(1:nfcst, function(x) indices)
      ## figure out number of blocks
      nblocks <- ceiling(nfcst / blocklength)
      for (i in seq(1, nblocks)){
        ii <- seq((i - 1)*blocklength + 1, min(nfcst, i*blocklength))
        ind[ii] <- lapply(ind[ii], function(x) setdiff(x, ii))
      }
    }
  }
  
  return(ind)
}

#' NULL
#' 
#' @param obs vector of observations
#' @param ind list or matrix of dimension (n x nref) of indices 
#'   of the observations to be used for each forecast instance
#' 
#' @export
generateRef <- function(obs, ind) {
  nobs <- length(obs)
  stopifnot(range(ind)[1] >= 1 & range(ind)[2] <= nobs)
  stopifnot(is.list(ind) | is.matrix(ind))
  
  if (is.list(ind)){
    nmax <- max(sapply(ind, length), 2)
    ind <- t(sapply(ind, function(x) c(x, rep(NA, nmax - length(x)))))
    ind <- ind[,!apply(is.na(ind), 2, all), drop=F]
  }
  
  return(array(obs[ind], dim(ind)))
}