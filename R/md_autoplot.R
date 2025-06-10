

md_autoplot_ <- function(x, xnm, ...) {
  txt <- Sprintf(x) # S3 generic [Sprintf()]
  ret <- c(
    txt,
    '\n',
    '```{r}',
    (attr(x, which = 'fig.height', exact = TRUE) %||% 4) |> sprintf(fmt = '#| fig-height: %.1f'),
    (attr(x, which = 'fig.width', exact = TRUE) %||% 7) |> sprintf(fmt = '#| fig-width: %.1f'),
    sprintf(fmt = '(%s) |> autoplot()', xnm),
    '```'
  )
  bib <- txt |> attr(which = 'bibentry', exact = TRUE)
  if (length(bib)) attr(ret, which = 'bibentry') <- bib
  return(ret)
}


#' @rdname md_
#' @examples
#' library(lcmm.tzh); list(
#'  '`lcmm`' = m20
#' ) |> render_(file = 'lcmm')
#' @export md_.lcmm
#' @export
md_.lcmm <- md_autoplot_
