# EnsCorr.R ensemble mean correlation
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