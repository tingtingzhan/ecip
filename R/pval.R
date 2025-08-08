
#' @title Retrieve \eqn{p}-values from Regression Model
#' 
#' @description ..
#' 
#' @param x an R regression model
#' 
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
#' @export
.pval.coeftest <- function(x) {
  ret <- x[,'Pr(>|z|)']
  names(ret) <- rownames(x)
  return(ret)
}



