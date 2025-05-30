

#' @title Variables in Terms
#' 
#' @param x an `R` model fit
#' 
#' @keywords internal
#' @name vterms
#' @export
vterms <- function(x) UseMethod(generic = 'vterms')

#' @rdname vterms
#' @examples
#' library(ordinal)
#' m = clmm(SURENESS ~ PROD + (1|RESP) + (1|RESP:PROD), data = soup,
#'  link = 'probit', threshold = 'equidistant')
#' m |> vterms()
#' @importFrom stats formula terms
#' @export vterms.default
#' @export
vterms.default <- function(x) {
  
  tmp. <- x |>
    formula() |> # must!!  terms.* of many mixed effect models only returns fixed effect
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



