# EnsRocss.R compute ROC area skill scores
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

#' Skill score for area under the ROC curve
#' 
#' Computes the skill score for the area under the ROC curve
#' 
#' @param ens n x j matrix of n probability forecasts for j categories
#' @param ens.ref placeholder for consistency with skill scores in SpecsVerification
#' @param obs n x j matrix of occurence of n verifying observations in j categories
#' 
#' @export
EnsRocss <- function(ens, ens.ref, obs){
  roc.area <- EnsRoca(ens, obs)
  return(lapply(roc.area, function(x) 2*x - 1))
}