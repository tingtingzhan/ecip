

#' @title User-Related Confidence Interval
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @param level \link[base]{double} scalar, default `.95`
#' 
#' @param ... additional parameters for S3 dispatch
#' 
#' @returns 
#' Function [confint_()] returns 
#' a \link[base]{ncol}-`2L` \link[base]{double} \link[base]{matrix}, 
#' with additional \link[base]{attributes},
#' \describe{
#' \item{`attr(.,'conf.level')`}{\link[base]{double} scalar, the argument `level`}
#' }
#' 
#' @keywords internal
#' @name confint_
#' @export
confint_ <- function(x, level, ...) UseMethod(generic = 'confint_')

#' @rdname confint_
#' @importFrom stats confint
#' @export confint_.default
#' @export
confint_.default <- function(x, level = .95, ...) {
  ci <- confint(x, level = level, ...) # let err
  if (is.data.frame(ci)) stop('do not allow')
  if (is.vector(ci, mode = 'double') && length(ci) == 2L) {
    attr(ci, which = 'dim') <- c(1L, 2L)
  }
  attr(ci, which = 'conf.level') <- level
  return(ci)
}


# tzh does not plan to defined a class of 'confint', yet.
#' @export
format.confint <- function(x, fmt = '%.2f', sep = ', ', ...) {
  out <- x
  storage.mode(out) <- 'character'
  out[] <- sprintf(fmt = fmt, x)
  out[out == 'NA'] <- '?'
  out[out == '-Inf'] <- '-\u221e'
  out[out == 'Inf'] <- '\u221e'
  paste(out[,1L], out[,2L], sep = sep)
}









