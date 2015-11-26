# EnsIgn.R compute area under the ROC curve
#
#     Copyright (C) 2015 MeteoSwiss
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

#' Ignorance score
#' 
#' Computes the ignorance score for an interpretation of the ensemble as a
#' probability forecast
#' 
#' @param ens n x j matrix of n probability forecasts for j categories
#' @param obs n x j matrix of occurence of n verifying observations in j categories
#' @param type selection of plotting positions to convert ensemble counts to 
#'   probabilities (default to 3, see \code{\link{count2prob}}
#' 
#' @references 
#' Wilks, D.S. (2011). Statistical methods in the atmospheric sciences (Third Edition). 
#' Academic press. 
#' Jolliffe, I.T. and D.B. Stephenson (2012). Forecast Verification. A Practitioner's Guide
#' in Atmospheric Science. Wiley-Blackwell.
#' 
#' @examples
#' tm <- toymodel()
#' 
#' ## compute ROC area for tercile forecasts using veriApply
#' veriApply("EnsIgn", fcst=tm$fcst, obs=tm$obs, prob=1:2/3)
#' 
#' @seealso \code{\link{veriApply}}, \code{\link{EnsIgnss}}, \code{\link{count2prob}}
#' 
#' @export
EnsIgn <- function(ens, obs, type=3, ...){
  stopifnot(is.matrix(ens), is.matrix(obs), length(obs) == length(ens))
  ens.prob <- count2prob(ens, type=type)
  ign <- -log2(ens.prob[as.logical(obs)])
  return(ign)
}