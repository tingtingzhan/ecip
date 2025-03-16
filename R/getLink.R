

#' @title Get link function of a model fit
#' 
#' @description Get link function of a model fit,
#' and determine whether the model is using the canonical link of the family!
#' 
#' @param x see **Usage**
#' 
#' @details 
#' Not sure how useful `marked:::inverse.link` will be in future!
#' 
#' @name getLink
#' @export
getLink <- function(x) UseMethod(generic = 'getLink')

#' @rdname getLink
#' @importFrom stats family
#' @export getLink.default
#' @export
getLink.default <- function(x) {
  
  if (!inherits(x, what = 'zeroinfl')) {
    lk <- tryCatch(
      expr = getElement(object = x, name = 'link'), 
      # S4 missing slot causes error
      error = function(e) return(character())
    )
    if (length(lk)) return(lk)
  } # I want pscl::zerolink go through the pipeline `x |> family() |> getLink()`
  
  x |>
    family() |>
    getLink() # beautiful!!!
  
} 



#' @rdname getLink
#' @export getLink.listof
#' @export
getLink.listof <- function(x) x |> lapply(FUN = getLink)










#' @title getCanonicalLink
#' 
#' @description ..
#' 
#' @param x ..
#' 
#' @examples
#' getCanonicalLink(binomial)
#' getCanonicalLink('binomial')
#' @name getCanonicalLink
#' @export
getCanonicalLink <- function(x) UseMethod(generic = 'getCanonicalLink') 

#' @rdname getCanonicalLink
#' @importFrom stats family
#' @export
getCanonicalLink.default <- function(x) x |> family() |> getCanonicalLink()

#' @rdname getCanonicalLink
#' @export
getCanonicalLink.family <- function(x) (x$family) |> getCanonicalLink.character()

#' @rdname getCanonicalLink
#' @export
getCanonicalLink.function <- function(x) {
  z <- x |> 
    do.call(args = list()) # all defaults; 'canonical'
  z$link
}

#' @rdname getCanonicalLink
#' @export
getCanonicalLink.character <- getCanonicalLink.function
