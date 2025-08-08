

#' @title Get Link Function of a Model Fit
#' 
#' @description Get link function of a model fit,
#' and determine whether the model is using the canonical link of the family!
#' 
#' @param x see **Usage**
#' 
#' @details 
#' Not sure how useful `marked:::inverse.link` will be in future!
#' 
#' @keywords internal
#' @name getLink
#' @export
getLink <- function(x) UseMethod(generic = 'getLink')

#' @rdname getLink
#' @importFrom stats family
#' @export getLink.family
#' @export
getLink.family <- function(x) {
  getElement(object = x, name = 'link')
  # S4 missing slot causes error; let err
  #lk <- tryCatch(
  #  expr = getElement(object = x, name = 'link'), 
  #  error = \(e) return(character())
  #)
  #if (length(lk)) return(lk)
} 


#' @rdname getLink
#' @importFrom stats family
#' @export getLink.default
#' @export
getLink.default <- function(x) x |> family() |> getLink()
# include ?pscl::zeroinfl return









#' @title Get Canonical Link Function of a Model Fit or \link[stats]{family}
#' 
#' @description ..
#' 
#' @param x an R object of model fit, or \link[stats]{family}
#' 
#' @returns
#' Function [getCanonicalLink()] returns a \link[base]{character} scalar.
#' 
#' @examples
#' getCanonicalLink(binomial)
#' getCanonicalLink('binomial')
#' @keywords internal
#' @name getCanonicalLink
#' @export
getCanonicalLink <- function(x) UseMethod(generic = 'getCanonicalLink') 

#' @rdname getCanonicalLink
#' @importFrom stats family
#' @export getCanonicalLink.default
#' @export
getCanonicalLink.default <- function(x) x |> family() |> getCanonicalLink()

#' @rdname getCanonicalLink
#' @export getCanonicalLink.family
#' @export
getCanonicalLink.family <- function(x) (x$family) |> getCanonicalLink.character()

#' @rdname getCanonicalLink
#' @export getCanonicalLink.function
#' @export
getCanonicalLink.function <- function(x) {
  z <- x |> 
    do.call(args = list()) # all defaults; 'canonical'
  z$link
}

#' @rdname getCanonicalLink
#' @export getCanonicalLink.character
#' @export
getCanonicalLink.character <- getCanonicalLink.function
