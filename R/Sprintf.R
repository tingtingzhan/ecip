
#' @title String Formatting of Statistical Models
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [Sprintf()] returns a \link[base]{noquote} \link[base]{character} scalar
#' 
#' @keywords internal
#' @name Sprintf
#' @export
Sprintf <- function(x) UseMethod(generic = 'Sprintf')




#' @rdname Sprintf
#' @importFrom stats formula
#' @importFrom rmd.tzh pkg_text
#' @export Sprintf.default
#' @export
Sprintf.default <- function(x) {
  
  fixedfom <- if (inherits(x, what = 'clmm')) {
    x$terms # formula(), which dispatch to ?stats:::formula.default, gives fixed and random effect
  } else if (inherits(x, what = 'merMod')) {
    formula(x, fixed.only = TRUE) # ?lme4:::formula.merMod
  } else {
    # ?nlme:::formula.lme returns fixed effect
    # ?nlme:::formula.gls returns fixed effect
    formula(x) # all other non-mixed-effect models
  }
  
  xvar <- (fixedfom[[3L]]) |> all.vars() |> unique.default()
  
  xfam <- tryCatch(x |> family(), error = identity)
  model_name <- if (inherits(xfam, what = 'error')) {
    paste(desc_(x), 'model')
  } else if (!length(xfam)) {
    paste(desc_(x), 'model')
  } else if (inherits(xfam, what = 'listof')) {
    # i.e., from ?pscl::zeroinfl
    paste(desc_(x), 'model')
  } else if (isS4(xfam)) {
    paste(desc_(x), 'model')
  } else if ((xfam$family != 'gaussian') || (xfam$link != 'identity')) {
    paste(desc_(x), 'model with', xfam |> desc_.family())
  } else paste(desc_(x), 'model')
    
  sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by fitting a %svariable %s using %s.', 
    x |> endpoint() |> vapply(FUN = deparse1, FUN.VALUE = '') |> unique() |> paste(collapse = '; '), # is.symbol(endpoint) compatible
    paste0('`', xvar, '`', collapse = ', '),
    nobsText(x),
    if (length(xvar) > 1L) 'multi' else 'uni',
    model_name,
    x |> pkg_text()
  )
  
}




