

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
#' \item{***Default*** [dataClasses.default()]}{`x |> terms() |> dataClasses.terms()`.}
#' 
#' }
#' 
#' Therefore, 
#' \itemize{
#' \item{For object that has \link[stats]{terms} dispatch but no \link[stats]{model.frame} dispatch, 
#' we use the default pipeline and do *not* write a `model.frame.*` function, e.g., `nlme::lme` and `nlme::gls` objects}
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








