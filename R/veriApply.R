#' Apply verification metrics from Specs-Verification to large datasets
#' 
#' @param verifun Name of function to compute verification metric (score, skill score)
#' @param fcst array of forecast values (at least 2-dimensional)
#' @param obs array or vector of verifying observations
#' @param tdim index of dimension with the different forecasts
#' @param ensdim index of dimension with the different ensemble members
#' @param prob probability threshold for category forecasts
#' @param threshold absolute threshold for category forecasts
#' 
#' @keywords utilities
#' @export
#' 
veriApply <- function(verifun, fcst, obs, tdim=5, ensdim=4, prob=NULL, threshold=NULL, na.rm=FALSE, ...){
  
  ## check function that is supplied
  stopifnot(exists(verifun))
  stopifnot(is.function(get(verifun)))
  
  ## check dimensions of input
  nfdims <- length(dim(fcst))
  nodims <- length(dim(obs))
  otdim <- min(nodims, tdim)
  ## check dimensions
  stopifnot(c(ensdim, tdim) <= nfdims)
  stopifnot(dim(obs) == dim(fcst)[-ensdim])

  ## make sure that forecasts (years) and ensembles are last in forecast array
  if (ensdim != nfdims | tdim != nfdims - 1){
    fcst <- aperm(fcst, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
  }
  if (otdim != nodims){
    obs <- aperm(obs, c(setdiff(1:nodims, otdim), otdim))    
  }
  
  ## dimensions of array to compute scores
  nens <- tail(dim(fcst), 1)
  ntim <- tail(dim(obs), 1)
  nrest <- length(obs)/ntim
  
  ## run the function
  xall <- array(c(fcst, obs), c(nrest, ntim, nens+1))
  ## mask missing values
  if (na.rm) {
    xmask <- apply(!is.na(xall), 1, any) & apply(!is.na(xall[,,nens+1]), 1, any)
  } else {
    xmask <- apply(!is.na(xall), 1, all)
  }
  ## check whether there are complete forecast/observation pairs at all
  stopifnot(any(xmask))
  
  ## run the workhorse
  out <- t(apply(xall[xmask,,], 
                 MARGIN=1, 
                 FUN=veriUnwrap, 
                 verifun=verifun, prob=prob, threshold=threshold, ...))
  
  ## reformat the output by converting to list
  if (is.list(out)){
    lnames <- names(out[[1]])
    olist <- list()
    for (ln in lnames) olist[[ln]] <- sapply(out, function(x) x[[ln]])
  } else {
    olist <- list(out)
  }
  
  ## re-expand the forecasts to account for missing values
  maskexpand <- rep(NA, length(xmask))
  maskexpand[xmask] <- 1:sum(xmask)
  olist <- lapply(olist, function(x) as.matrix(x)[maskexpand,])
  
  ## rearrange output to original dimensions
  out <- lapply(olist, function(x){
    if (length(x) == length(fcst)){
      ## repermute the output
      fperm <- 1:nfdims
      fperm[c(tdim, ensdim)] <- nfdims - 1:0
      xout <- aperm(array(x, dim(fcst)), fperm)
    } else if (length(x) == length(obs)){
      operm <- 1:nodims
      operm[otdim] <- nodims
      xout <- aperm(array(x, dim(obs)), operm)
    } else if (length(x) == prod(dim(obs)[-nodims])) {
      xout <- array(x, dim(obs)[-nodims])
    } else {
      xout <- x
    }
    return(xout)
  })
  
  ## if output list is of length one, output object within list
  if (length(out) == 1) out <- out[[1]]
  
  return(out)
  
}