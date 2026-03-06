

#' @title Variables in Terms
#' 
#' @param x an `R` model fit
#' 
#' @examples
#' # see ?ordinal::clmm examples
#' (SURENESS ~ PROD + (1|RESP) + (1|RESP:PROD)) |>
#'   vterms()
#' @keywords internal
#' @export
vterms <- function(x) UseMethod(generic = 'vterms')

#' @export
vterms.default <- function(x) {
  x |>
    formula() |> # must!!  terms.* of many mixed effect models only returns fixed effect
    vterms.formula()
}

#' @export
vterms.formula <- function(formula) {
  
  tmp. <- formula |> 
    terms() |> # ?stats:::terms.terms handles exception :)
    attr(which = 'variables', exact = TRUE) |>
    as.list.default()
  tmp <- tmp.[-1L] # remove first element of quote(list)
  
  is_group <- vapply(tmp, FUN = \(i) {
    if (is.symbol(i)) return(FALSE)
    i[[1]] == '|'
  }, FUN.VALUE = NA)
  
  ret <- tmp[!is_group]
  attr(ret, which = 'group') <- tmp[is_group]
  return(ret)
  
}


