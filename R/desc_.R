
#' @title Model Description
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @returns
#' The function [desc_()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
desc_ <- function(x) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'desc_')
}

#' @export
desc_.default <- function(x) stop('write desc_.', class(x)[1L])

#' @export
desc_.family <- function(x) {
  
  lnk <- getLink(x)
  clnk <- getCanonicalLink(x)
  lk <- if (lnk != clnk) paste0('(', lnk, '-link)') # else NULL
  
  x$family |> 
    switch(
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
      }
    ) |>
    paste0('-response ', lk) |> 
    trimws()
  
}






