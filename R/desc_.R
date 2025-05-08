
#' @title Model Description
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @returns
#' Function [desc_()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
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



#' @rdname desc
#' @export desc_.family
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






