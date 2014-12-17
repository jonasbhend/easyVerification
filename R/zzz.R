.onAttach <- function(...) {
  pkgname <- "easyVerification"
  lib <- system.file(package = pkgname)
  ver <- packageDescription(pkgname)$Version
  url <- "https://raw.githubusercontent.com/jonasbhend/easyVerification/master/DESCRIPTION"
  con <- tryCatch(getURL(url, ssl.verifypeer = FALSE), error = function(er) {
    er <- NULL
    return(er)
  })
  if (!is.null(con)) {
    b <- readLines(textConnection(con))
    latest.ver <- package_version(gsub("Version: ", "", b[grep("Version", b)]))
    if (ver < latest.ver) {
      ver.mess1 <- paste("WARNING: Your current version of", pkgname,"is not up-to-date")
      ver.mess <- paste("Get the latest version", latest.ver, 'using install_github("jonasbhend/easyVerification")')      
      packageStartupMessage(ver.mess1)
      packageStartupMessage(ver.mess)
    }
  }
} 
# End
