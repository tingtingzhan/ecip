

#' @importFrom methods new
md_autoplot_ <- function(x, xnm, ...) {
  
  z1 <- Sprintf(x) # S3 generic [Sprintf()]
    
  z2 <- c(
    '```{r}',
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    
    sprintf(fmt = '(%s) |> ggplot2::autoplot()', xnm),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}


#' @rdname md_
#' @examples
#' library(lcmm.tzh); list(
#'  '`lcmm`' = m20
#' ) |> render_(file = 'lcmm')
#' @export md_.lcmm
#' @export
md_.lcmm <- md_autoplot_
