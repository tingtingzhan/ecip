


# e.g., ?stats:::summary.mlm, returns 'listof' 'summary.lm'








#' @title Convert `'listof'` to \link[flextable]{flextable}
#' 
#' @param x `'listof'`
#' 
#' @param ... ..
#' 
#' @note
#' This is quite exploratory
#' 
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.listof
#' @export
as_flextable.listof <- function(x, ...) {
  
  x[[1L]] |>
    class() |>
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
      
      
      
    })
  
}

#' @rdname Sprintf
#' @export Sprintf.listof
#' @export
Sprintf.listof <- function(x) character()


#' @rdname getLink
#' @export getLink.listof
#' @export
getLink.listof <- function(x) x |> lapply(FUN = getLink)




#' @rdname pval
#' @export .pval.listof
#' @export
.pval.listof <- function(x) x |> lapply(FUN = .pval)





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
    
  #.Defunct(msg = '[md_.listof]: where is this used?')
  
  z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}
  








