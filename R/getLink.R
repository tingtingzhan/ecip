

#' @title Get Link Function of a Model Fit
#' 
#' @description Get link function of a model fit,
#' and determine whether the model is using the canonical link of the family!
#' 
#' @param x see **Usage**
#' 
#' @keywords internal
#' @name getLink
#' @export
getLink <- function(x) UseMethod(generic = 'getLink')

#' @export
getLink.family <- function(x) {
  getElement(object = x, name = 'link')
  # S4 missing slot causes error; let err
} 

#' @export
getLink.default <- function(x) {
  x |> 
    family() |> 
    getLink()
  # include ?pscl::zeroinfl return
}








#' @title Get Canonical Link Function of a Model Fit or \link[stats]{family}
#' 
#' @description ..
#' 
#' @param x an R object of model fit, or \link[stats]{family}
#' 
#' @returns
#' The function [getCanonicalLink()] returns a \link[base]{character} scalar.
#' 
#' @examples
#' getCanonicalLink(binomial)
#' getCanonicalLink('binomial')
#' @keywords internal
#' @export
getCanonicalLink <- function(x) UseMethod(generic = 'getCanonicalLink') 

#' @export
getCanonicalLink.default <- function(x) x |> family() |> getCanonicalLink()

#' @export
getCanonicalLink.family <- function(x) (x$family) |> getCanonicalLink.character()

#' @export
getCanonicalLink.function <- function(x) {
  z <- x |> 
    do.call(args = list()) # all defaults; 'canonical'
  z$link
}

#' @export
getCanonicalLink.character <- getCanonicalLink.function
