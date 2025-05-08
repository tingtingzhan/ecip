
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


pkgText <- function(x) {
  if (isS4(x)) {
    pkg_txt <- x |> 
      class() |> 
      attr(which = 'package', exact = TRUE) |> 
      sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
    return(pkg_txt)
  } 
    
  f <- getCall(x)[[1L]]
  pkg <- tryCatch(expr = {
    f |>
      eval() |> # function call could be un-exported, e.g., nlme:::lme.formula, and err
      environment() |>
      getNamespaceName()
  }, error = \(e) {
    aw <- f |>
      as.character() |>
      getAnywhere()
    if (length(aw$where) > 1L) stop('really shouldnt happen...')
    (aw$where) |>
      gsub(pattern = '^namespace\\:', replacement = '')
  })
  # installed.packages(priority = 'base') |> rownames() # RStudio do not have a delete button for base-packages
  if (pkg %in% c('base', 'compiler', 'datasets', 'graphics', 'grDevices', 'grid', 'methods', 'parallel', 'splines', 'stats', 'stats4', 'tcltk', 'tools', 'utils')) {
    return('<u>**`R`**</u>')
  } 
  pkg |> sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
  
}


#' @rdname Sprintf
#' @importFrom stats formula getCall
#' @importFrom utils getAnywhere
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
    pkgText(x)
  )
  
}




