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