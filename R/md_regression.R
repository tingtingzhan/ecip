
#' @title Fast Markdown Lines for Regression Model
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @returns 
#' The function [md_regression_()] returns an S4 class `'md_lines'`.
#' 
#' @keywords internal
#' @importFrom fastmd fromPackage pkg_text
#' @export
md_regression_ <- function(x) {
  
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
  
  desc. <- desc_(x) |>
    new(Class = 'md_lines')
  
  model_name <- if (inherits(xfam, what = 'error')) {
    paste(desc., 'model')
  } else if (!length(xfam)) {
    paste(desc., 'model')
  } else if (inherits(xfam, what = 'listof')) {
    # i.e., from ?pscl::zeroinfl
    paste(desc., 'model')
  } else if (isS4(xfam)) {
    paste(desc., 'model')
  } else if ((xfam$family != 'gaussian') || (xfam$link != 'identity')) {
    tryCatch(expr = {
      paste(desc., 'model with', xfam |> desc_.family())
    }, error = \(e) {
      # MASS::glm.nb
      paste(desc., 'model')
    })
  } else paste(desc., 'model')
  
  print_endpoint <- \(e) {
    # `e` is returned value from `endpoint()`
    if (is.symbol(e)) return(as.character(e))
    if (is.language(e)) return(deparse1(e))
    if (is.recursive(e)) {
      ret <- e |> 
        vapply(FUN = deparse1, FUN.VALUE = '') |> 
        unique.default() |> 
        paste(collapse = '; ')
      return(ret)
    }
    stop('shouldnt come here')
  }
    
  pkg <- x |> fromPackage()
  
  sprintf(
    fmt = 'A %svariable %s analyzes the endpoint **`%s`** based on %s using %s.', 
    if (length(xvar) > 1L) 'multi' else 'uni',
    model_name,
    x |> endpoint() |> print_endpoint(),
    nobsText(x),
    pkg |> pkg_text()
  ) |>
    new(Class = 'md_lines', bibentry = desc.@bibentry, package = pkg)
  
}




