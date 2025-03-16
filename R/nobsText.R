
#' @title Text of number of observations
#' 
#' @description ..
#' 
#' @param x regression model
#' 
#' @export
nobsText <- function(x) {
  if (!length(x)) stop('do not allow') # return('')
  UseMethod(generic = 'nobsText')
}


#' @importFrom stats nobs
#' @export
nobsText.default <- function(x) {
  if (!length(n <- nobs(x))) stop(sprintf(fmt = 'write individual nobsText.%s function', class(x)[1L])) # return(invisible(character()))
  if (is.na(n)) return('') # e.g., ?vcd::Kappa
  return(paste0(n, ' subjects'))
}


#' @export
nobsText.glht <- function(x) nobsText(x$model)
# requires S3 generic [nobsText()]; do *not* move to \pkg{multcomp.tzh}



