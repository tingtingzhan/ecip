

#' @title R Markdown Lines by `ggplot2::autoplot`
#' 
#' @param x,xnm,... ..
#' 
#' @keywords internal
#' @export
md_autoplot_ <- function(x, xnm, ...) {
  
  z1 <- .md_reg(x)
    
  z2 <- c(
    '```{r}',
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    
    xnm |> sprintf(fmt = 'ggplot2::autoplot(%s)'),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?fastmd::c.md_lines
  
}


