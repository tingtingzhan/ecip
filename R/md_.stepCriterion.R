

#' @title md_.stepCriterion
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @importFrom rmd.tzh md_
#' @export md_.stepCriterion
#' @export
md_.stepCriterion <- function(x, xnm, ...) {
  
  xvar <- eval(parse(text = x$initial)) |> all.vars()
  x0 <- x$final.fit
  
  xfam <- tryCatch(x0 |> family(), error = identity)
  model_name <- if (inherits(xfam, what = 'error')) {
    paste(desc_(x0), 'model')
  } else if (!length(xfam)) {
    paste(desc_(x0), 'model')
  } else if ((xfam$family != 'gaussian') || (xfam$link != 'identity')) {
    paste(desc_(x0), 'model with', xfam |> desc_.family())
  } else paste(desc_(x0), 'model')
  
  txt <- sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by fitting a %svariable %s using %s.  %s stepwise variable selection is performed by %s.', 
    x0 |> endpoint() |> vapply(FUN = deparse1, FUN.VALUE = '') |> unique() |> paste(collapse = '; '), # is.symbol(endpoint) compatible
    paste0('`', xvar, '`', collapse = ', '),
    nobsText(x0),
    if (length(xvar) > 1L) 'multi' else 'uni',
    model_name,
    pkgText(x0),
    switch(EXPR = x$direction, backward = 'Backward', forward = 'Forward'),
    switch(
      EXPR = x$criterion, # ?stepCriterion.glmgee
      'P(Chisq>)(*)' = '$p$-values of selected predictors',
      AGPC = 'Akaike-type penalized Gaussian pseudo-likelihood criterion'
    )
  )
  
  c(
    txt, 
    '```{r}', 
    sprintf(fmt = '(%s$final.fit) |> ecip() |> as_flextable.ecip()', xnm),
    '```',
    '<any-text>'
  )
  
  
}