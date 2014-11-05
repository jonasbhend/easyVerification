#' Apply verification metrics from Specs-Verification to large datasets
#' 
#' @param verifun Name of function to compute verification metric (score, skill score)
#' @param fcst array of forecast values (at least 2-dimensional)
#' @param obs array or vector of verifying observations
#' @param tdim index of dimension with the different forecasts
#' @param ensdim index of dimension with the different ensemble members
#' @param prob probability threshold for category forecasts
#' @param threshold absolute threshold for category forecasts
#' @param na.rm logical, should incomplete forecasts be used?
#' @param ... additional arguments passed to \code{verifun}
#' 
#' @examples
#' obs <- array(rnorm(1000*30), c(1000,30))
#' fcst <- array(rnorm(1000*30*50), c(1000, 30, 50)) + 0.2*as.vector(obs)
#' f.me <- veriApply('EnsMe', fcst, obs)
#' 
#' 
#' @keywords utilities
#' @export
#' 
veriApply <- function(verifun, fcst, obs, tdim=length(dim(fcst)) - 1, ensdim=length(dim(fcst)), prob=NULL, threshold=NULL, na.rm=FALSE, ...){
  
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
  stopifnot(odims == dim(fcst)[-ensdim])
  stopifnot(odims[-otdim] == dim(fcst)[-c(ensdim, tdim)])
  
  ## make sure that forecasts (years) and ensembles are last in forecast array
  if (ensdim != nfdims | tdim != nfdims - 1){
    fcst <- aperm(fcst, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
  }
  if (otdim != nodims){
    obs <- aperm(obs, c(setdiff(1:nodims, otdim), otdim))    
  }
  
  ## dimensions of array to compute scores
  nens <- tail(dim(fcst), 1)
  ntim <- head(tail(dim(fcst), 2), 1)
  nrest <- length(obs)/ntim
  
  ## run the function
  xall <- array(c(fcst, obs), c(nrest, ntim, nens+1))
  ## mask missing values
  if (na.rm) {
    xmask <- apply(apply(!is.na(xall), 1:2, all), 1, any)
  } else {
    xmask <- apply(!is.na(xall), 1, all)
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
                       verifun=verifun, prob=prob, threshold=threshold, ...))
  
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