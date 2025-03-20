

#' @title dataClasses
#' 
#' @description ..
#' 
#' @param x a regression model
#' 
#' @returns 
#' S3 generic function [dataClasses()] returns a named \link[base]{character} \link[base]{vector}.
#' 
#' @details
#' Function [dataClasses()] is inspired by the attribute `'dataClasses'` of
#' \link[stats]{terms} object.
#' There are two possible pipelines to achieve this purpose,
#' \describe{
#' 
#' \item{***Default*** [dataClasses.default()]}{`x |> terms() |> dataClasses.terms()`.
#' Minor variation(s) include,
#' \itemize{
#' \item{[dataClasses.zeroinfl()] for \link[pscl]{zeroinfl} object, 
#' as `pscl:::terms.zeroinfl()` return is not desired}
#' }
#' }
#' 
#' \item{***Alternative***}{`x |> model.frame() |> attr(which = 'terms', exact = TRUE) |> dataClasses.terms()`, 
#' e.g., 
#' \itemize{
#' \item{[dataClasses.merMod()] for \link[lme4]{merMod} object, 
#' as `lme4:::terms.merMod()` return does not have `'dataClasses'` attribute}
#' }
#' }
#' }
#' 
#' Therefore, 
#' \itemize{
#' \item{For object that has \link[stats]{terms} dispatch but no \link[stats]{model.frame} dispatch, 
#' we use the default pipeline and do *not* write a `model.frame.*` function, e.g., \link[nlme]{lme} and \link[nlme]{gls} objects}
#' }
#' 
#' @name dataClasses
#' @export
dataClasses <- function(x) UseMethod(generic = 'dataClasses')

#' @rdname dataClasses
#' @importFrom stats terms
#' @export dataClasses.default
#' @export
dataClasses.default <- function(x) x |> terms() |> dataClasses.terms()

#' @rdname dataClasses
#' @importFrom DemographicTable msg_logical
#' @export dataClasses.terms
#' @export
dataClasses.terms <- function(x) { # primary work horse!!!
  ret <- attr(x, which = 'dataClasses', exact = TRUE) %||% 
    stop('`terms` object does not have `dataClasses` attribute?')
  if (any(ret[-1L] == 'logical')) warning(msg_logical()) # first element is endpoint
  return(ret)
}




# requires [dataClasses.terms()]; do *not* move to \pkg{lme4.tzh}
#' @rdname dataClasses
#' @importFrom stats model.frame
#' @export dataClasses.merMod
#' @export
dataClasses.merMod <- function(x) {
  x |>
    model.frame() |> # ?lme4:::model.frame.merMod
    attr(which = 'terms', exact = TRUE) |>
    # do *not* overwrite ?lme4:::terms.merMod (which does not have dataClasses)
    dataClasses.terms()
  # seems wrong for 'nlmerMod' object..
  # 'glmerMod' and 'lmerMod' should be correct
}


# requires [dataClasses.terms()]; do *not* move to \pkg{pscl.tzh}
#' @rdname dataClasses
#' @export dataClasses.zeroinfl
#' @export
dataClasses.zeroinfl <- function(x) {
  (x$terms$full) |> 
    # do *not* overwrite ?pscl:::terms.zeroinfl; packageDate('pscl') # 2024-01-14
    dataClasses.terms()
}






