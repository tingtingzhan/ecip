

#' @title Convert \link[glmtoolbox.tzh]{backwardCriterion} to \link[base]{matrix}
#' 
#' @param x returned object from function \link[glmtoolbox.tzh]{backwardCriterion}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @method as.matrix backwardCriterion
#' @export as.matrix.backwardCriterion
#' @export
as.matrix.backwardCriterion <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  z <- list(
    x |> attr(which = 'initial.fit', exact = TRUE),
    x$final.fit
  ) |> 
    lapply(FUN = ecip)
    
  m <- list(
    z[[1L]] |> as.matrix.ecip(type = 'p_only'), # `initial` model, only print p-values
    z[[2L]] |> as.matrix.ecip(type = 'ncol1') # `final` model
  )

  r <- m |>
    lapply(FUN = rownames) |>
    unlist(use.names = FALSE) |>
    unique.default() # do *not* ?base::sort !!!
  
  out <- array(data = '\u274c', dim = c(length(r), 2L), dimnames = list(
    r, 
    paste(c('(Initial)', '(Final)'), vapply(m, FUN = colnames, FUN.VALUE = ''), sep = '\n')
  ))
  for (i in 1:2) {
    out[match(x = rownames(m[[i]]), table = r), i] <- m[[i]]
  }
  
  attr(out, which = 'row.title') <- x$final.fit |> endpoint() |> deparse1()
  return(out)
  
}



#' @title Convert \link[glmtoolbox.tzh]{backwardCriterion} to \link[flextable]{flextable}
#' 
#' @param x returned object from function \link[glmtoolbox.tzh]{backwardCriterion}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.backwardCriterion
#' @export
as_flextable.backwardCriterion <- function(
    x, 
    row.title = z |> attr(which = 'row.title', exact = TRUE),
    ...
) {
  z <- as.matrix.backwardCriterion(x, ...)
  z |>
    as_flextable.matrix(
      row.title = row.title,
    ) |>
    color(j = 2L, color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      x |> textCriterion() |> sprintf(fmt = '\u274c: predictor(s) removed by %s')
    ))
}



#' @rdname Sprintf
#' @importFrom glmtoolbox.tzh textCriterion
#' @importFrom rmd.tzh pkg_text
#' @export Sprintf.backwardCriterion
#' @export
Sprintf.backwardCriterion <- function(x) {
  
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
    x0 |> nobsText(),
    if (length(xvar) > 1L) 'multi' else 'uni',
    model_name,
    x0 |> pkg_text(),
    switch(EXPR = x$direction, backward = 'Backward', forward = 'Forward'),
    textCriterion(x)
  )
  
}


#' @rdname md_
#' @export md_.backwardCriterion
#' @export
md_.backwardCriterion <- md_flextable_

