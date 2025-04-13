

md_autoplot_ <- function(x, xnm, ...) {
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  return(c(
    Sprintf(x), # S3 generic [Sprintf()]
    '\n',
    sprintf(fmt = '```{r fig.height = %.1f, fig.width = %.1f}', h, w),
    sprintf(fmt = '(%s) |> autoplot()', xnm),
    '```', 
    '<any-text>'))
}

#' @rdname md_
#' @examples
#' data(mayo, package = 'survivalROC'); list(
#'  'survival_roc' = mayo |> 
#'   within.data.frame(expr = {
#'    time = as.difftime(time, units = 'days')
#'    edp = Surv(time, censor)
#'   }) |>
#'   survival_roc(formula = edp ~ mayoscore4, predict.time = 365)
#' ) |> render_(file = 'survival_roc')
#' @export
md_.survival_roc <- md_autoplot_

#' @rdname md_
#' @examples
#' library(lcmm.tzh); list(
#'  '`lcmm`' = m20
#' ) |> render_(file = 'lcmm')
#' @export md_.lcmm
#' @export
md_.lcmm <- md_autoplot_
