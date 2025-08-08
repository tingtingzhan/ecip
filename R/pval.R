
#' @title Retrieve \eqn{p}-values from Regression Model
#' 
#' @description ..
#' 
#' @param x an R regression model
#' 
#' @keywords internal
#' @name pval
#' @export
.pval <- function(x) {
  if (!length(x)) stop('do not allow NULL input')
  UseMethod(generic = '.pval')
}

#' @rdname pval
#' @importFrom lmtest coeftest
#' @export .pval.default
#' @export
.pval.default <- function(x) {
  tryCatch(expr = {
    x |> summary() |> .pval()
  }, error = \(e) {
    tryCatch(expr = {
      x |> coeftest() |> .pval() # [.pval.coeftest()]
    }, error = \(e) double())
  })
}



# \pkg{lmtest} and \pkg{AER} same author
# ?lmtest::coeftest is the S3 generic, and has some dispatches
# ?lmtest:::coeftest.glm
# but other dispatches are in
# ?AER:::coeftest.polr
# ?AER:::coeftest.multinom
# \link[lmtest]{coeftest.default} does not have parameter `alternative`
# ?lmtest:::coef.coeftest good enough, since at least 2022-03-21
#' @rdname pval
#' @export .pval.coeftest
#' @export
.pval.coeftest <- function(x) {
  ret <- x[,'Pr(>|z|)']
  names(ret) <- rownames(x)
  return(ret)
}




# e.g.,
# ?stats:::summary.mlm returns 'listof' 'summary.lm'
#' @rdname pval
#' 
#' @details
#' Function [.pval.listof()] applies to `'mlm' |> stats:::summary.mlm() |> .pval.listof()`.
#' 
#' @export .pval.listof
#' @export
.pval.listof <- function(x) {
  x |> lapply(FUN = .pval)
}




