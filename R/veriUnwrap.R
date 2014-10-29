veriUnwrap <- function(x, verifun, prob=NULL, threshold=NULL, ...){
  nn <- ncol(x)
  vfun <- get(verifun)
  ## mask missing values
  xmask <- apply(!is.na(x), 1, all)
  x <- x[xmask,]
  ## check whether this is a skill score or a score
  is.skill <- substr(verifun, nchar(verifun) - 1, nchar(verifun)) == 'ss'
  if (is.skill){
    xclim <- t(array(x[,nn], c(nrow(x), nrow(x))))
    out <- vfun(convert2prob(x[,-nn], prob=prob, threshold=threshold),
                convert2prob(xclim, prob=prob, threshold=threshold),
                convert2prob(x[,nn], prob=prob, threshold=threshold))
  } else {
    out <- vfun(convert2prob(x[,-nn], prob=prob, threshold=threshold),
                convert2prob(x[,nn], prob=prob, threshold=threshold))    
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