

#' @title \pkg{rmarkdown} Lines based on \link[flextable]{flextable}
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param font.size see function \link[flextable]{set_flextable_defaults}
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
md_.listof <- function(x, xnm, font.size, ...) {
  
  .Defunct(msg = '[md_.listof]: where is this used?')
  
  z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    if (!missing(font.size)) font.size |> sprintf(fmt = 'set_flextable_defaults(font.size = %.1f)'),
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    if (!missing(font.size)) 'init_flextable_defaults()',
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}




