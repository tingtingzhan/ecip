
#' @title Endpoint 
#' 
#' @description Find the endpoint of an R regression model.
#' 
#' @param x ..
#' 
#' @returns 
#' Function [endpoint()] returns a \link[base]{language} object.
#' 
#' @keywords internal
#' @name endpoint
#' @export
endpoint <- function(x) {
  if (!length(x)) stop('do not allow')
  UseMethod(generic = 'endpoint')
}

#' @rdname endpoint
#' @importFrom stats formula
#' @export endpoint.default
#' @export
endpoint.default <- function(x) {
  x |>
    formula() |> # use S3; let err
    endpoint.formula()
}


#' @rdname endpoint
#' @param formula \link[stats]{formula}
#' @examples 
#' endpoint(a ~ b)
#' # endpoint(a ~ b ~ c) # stop support
#' endpoint(a ~ b | c)
#' @export endpoint.formula
#' @export
endpoint.formula <- function(formula) {
  if (!is.call(formula) || (formula[[1L]] != '~')) stop('formula must be formula')
  if (length(formula) == 2L) return('')
  # return(.lhs(formula)) # I really seldom use `a~b~c` model. Write specific [endpoint.*()] for them
  return(formula[[2L]])
}



