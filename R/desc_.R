
#' @title Model Description
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @name desc
#' @export
desc_ <- function(x) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'desc_')
}

#' @rdname desc
#' @export desc_.default
#' @export
desc_.default <- function(x) stop('write desc_.', class(x)[1L])



link_text <- function(x) {
  lnk <- getLink(x)
  clnk <- getCanonicalLink(x)
  if (lnk == clnk) return(character())
  return(lnk)
}






family_text <- function(x) UseMethod('family_text')

#' @export
family_text.family <- function(x) family_text.character(x$family)

#' @export
family_text.character <- function(x) {
  switch(x, 
         # from glm, ?stats::family
         binomial = 'binary', 
         gaussian = 'Gaussian', 
         Gamma = 'Gamma',
         # inverse.gaussian ?
         poisson = 'Poisson', 
         geometric = 'Geometric', #?
         negbin = 'negative binomial', #?
         # end of from glm
         {
           if (startsWith(x, prefix = 'Negative Binomial')) 'negative binomial'
         })
}


#' @export
desc_.family <- function(x) {
  lk <- if (length(tmp <- link_text(x))) paste0('(', tmp, '-link)') 
  family_text(x) |>
    paste0('-response ', lk) |> 
    trimws()
}






