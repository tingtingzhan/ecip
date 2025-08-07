

#' @title R Markdown Lines by `ggplot2::autoplot`
#' 
#' @param x,xnm,... ..
#' 
#' @keywords internal
#' @importFrom methods new
#' @export
md_autoplot_ <- function(x, xnm, ...) {
  
  z1 <- Sprintf(x) # S3 generic [Sprintf()]
    
  z2 <- c(
    '```{r}',
    '#| echo: false', 
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


