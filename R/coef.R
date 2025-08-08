

#' @title User-Related Coefficient Estimates
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [coef_()] returns either a named \link[base]{numeric} \link[base]{vector},
#' or a \link[base]{list} of named \link[base]{numeric} \link[base]{vector}s.
#' 
#' @keywords internal
#' @name coef_
#' @export
coef_ <- function(x) UseMethod(generic = 'coef_')

#' @rdname coef_
#' @importFrom stats coef
#' @export
coef_.default <- function(x) {
  cf <- coef(x) # S3 generic
  if (is.matrix(cf)) stop('write individual S3 method!')
  return(cf)
}




