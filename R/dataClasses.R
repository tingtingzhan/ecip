

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
#' The function [dataClasses()] is inspired by `attr(, 'dataClasses')` of
#' \link[stats]{terms} object.
#' 
#' 
#' @examples
#' x = lm(breaks ~ tension + wool, data = warpbreaks)
#' x |> terms() |> attr(which = 'dataClasses')
#' dataClasses(x)
#' 
#' @keywords internal
#' @export
dataClasses <- function(x) UseMethod(generic = 'dataClasses')

#' @export
dataClasses.default <- function(x) {
  x |> 
    terms() |> 
    dataClasses.terms()
}

#' @export
dataClasses.terms <- function(x) { # primary work horse!!!
  attr(x, which = 'dataClasses', exact = TRUE) %||% 
    stop('`terms` object does not have `dataClasses` attribute?')
}


