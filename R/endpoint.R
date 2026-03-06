
#' @title Endpoint 
#' 
#' @description Find the endpoint of an R regression model.
#' 
#' @param x ..
#' 
#' @examples 
#' endpoint(a ~ b)
#' # endpoint(a ~ b ~ c) # stop support
#' endpoint(a ~ b | c)
#' 
#' @returns 
#' The function [endpoint()] returns a \link[base]{language} object.
#' 
#' @keywords internal
#' @name endpoint
#' @export
endpoint <- function(x) {
  if (!length(x)) stop('do not allow')
  UseMethod(generic = 'endpoint')
}

#' @export
endpoint.default <- function(x) {
  x |>
    formula() |> # use S3; let err
    endpoint.formula()
}


#' @export
endpoint.formula <- function(formula) {
  if (!is.call(formula) || (formula[[1L]] != '~')) stop('formula must be formula')
  if (length(formula) == 2L) return('')
  # return(.lhs(formula)) # I really seldom use `a~b~c` model. Write specific [endpoint.*()] for them
  return(formula[[2L]])
}



