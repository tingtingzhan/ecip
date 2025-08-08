
# e.g.,
# ?stats:::summary.mlm returns 'listof' 'summary.lm'
# tzh's [MASS.tzh::as.univar()] returns 'listof' regression models



#' @rdname pval
#' 
#' @details
#' Function [.pval.listof()] applies to `'mlm' |> stats:::summary.mlm() |> .pval.listof()`.
#' 
#' @export .pval.listof
#' @export
.pval.listof <- function(x) {
  x |> lapply(FUN = .pval)
}







#' @title Convert `'listof'` to \link[flextable]{flextable}
#' 
#' @param x a `'listof'` object
#' 
#' @param ... ..
#' 
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.listof
#' @export
as_flextable.listof <- function(x, ...) {
  
  .Defunct(msg = '[as_flextable.listof]: where is this used?')
  
  .class <- x |>
    lapply(FUN = class) |>
    unique.default()
  
  if (length(.class) > 1L) stop('all element must be of the same `class`')
  
  .class[[1L]][1L] |>
    
    switch(EXPR = _, ecip = {
      
      # i.e., a list of ?ltm::cronbach.alpha
      
      y <- x |> 
        lapply(FUN = \(i) {
          i |>
            intercept_rm.ecip() |>
            as.matrix.ecip()
        })
      
      y |> 
        do.call(what = rbind) |>
        as_flextable.matrix(
          row.title = x[[1L]]@endpoint,
          hline_i = y |> vapply(FUN = nrow, FUN.VALUE = NA_integer_) |> cumsum()
        )
      
    }, {
      
      stop('only accepts a `listof` `ecip`s, for now')
      
    })
  
}

#' @rdname Sprintf
#' @export Sprintf.listof
#' @export
Sprintf.listof <- function(x) {
  .Defunct(msg = '[Sprintf.listof]: where is this used?')
  character()
}

#' @rdname getLink
#' @export getLink.listof
#' @export
getLink.listof <- function(x) {
  .Defunct(msg = '[getLink.listof]: where is this used?')
  x |> lapply(FUN = getLink)
}






#' @title \pkg{rmarkdown} Lines based on \link[flextable]{flextable}
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' # none yet..
#' 
#' @keywords internal
#' @importFrom methods new
#' @export md_.listof
#' @export
md_.listof <- function(x, xnm, ...) {
    
  .Defunct(msg = '[md_.listof]: where is this used?')
  
  .class <- x |>
    lapply(FUN = class) |>
    unique.default()
  
  if (length(.class) > 1L) stop('all element must be of the same `class`')
  
  .class[[1L]][1L] |>
    
    switch(EXPR = _, ecip = {
      
      z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
        new(Class = 'md_lines')
      
      z2 <- c(
        '```{r}', 
        '#| echo: false', 
        xnm |> sprintf(fmt = 'as_flextable(%s)'),
        '```'
      ) |>
        new(Class = 'md_lines')
      
      return(c(z1, z2)) # ?rmd.tzh::c.md_lines
      
    }, {
      
      stop('only accepts a `listof` `ecip`s, for now')
      
    })
}
  








