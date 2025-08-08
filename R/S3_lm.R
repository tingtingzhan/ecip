
#' @rdname desc
#' @export desc_.lm
#' @export
desc_.lm <- function(x) 'ordinary least squares'

#' @rdname pval
#' @method .pval summary.lm
#' @export .pval.summary.lm
#' @export
.pval.summary.lm <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|t|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}



#' @rdname desc
#' @export desc_.glm
#' @export
desc_.glm <- function(x) 'generalized linear'


#' @rdname pval
#' @method .pval summary.glm
#' @export .pval.summary.glm
#' @export
.pval.summary.glm <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}





# return from ?stats::lm; with multiple endpoints
# note that we have ?stats:::vcov.mlm inside ?stats::confint.lm

#' @rdname desc
#' @export desc_.mlm
#' @export
desc_.mlm <- function(x) 'multivariate linear regression' # ?stats::lm with endpoint `cbind(., .)`


#' @rdname coef_
#' @export coef_.mlm
#' @export
coef_.mlm <- function(x) {
  # otherwise dispatch to ?stats:::coef.default
  cf <- x$coefficients # endpoints on columns
  ret <- dim(cf)[2L] |>
    seq_len() |>
    lapply(FUN = \(i) cf[,i])
  names(ret) <- dimnames(x$coefficients)[[2L]]
  return(ret)
}

#' @rdname confint_
#' @importFrom stats confint.lm
#' @export confint_.mlm
#' @export
confint_.mlm <- function(x, level = .95, ...) {
  # otherwise dispatch to ?stats::confint.lm
  ci <- confint.lm(x, level = level, ...)
  rnm <- dimnames(ci)[[1L]]
  edp <- dimnames(x$coefficients)[[2L]]
  ret <- lapply(paste0(edp, ':'), FUN = \(i) {
    out <- ci[startsWith(rnm, prefix = i), , drop = FALSE]
    rownames(out) <- gsub(i, replacement = '', x = rownames(out))
    attr(out, which = 'conf.level') <- level
    return(out)
  })
  names(ret) <- edp
  return(ret)
}


#' @name endpoint
#' @export endpoint.mlm
#' @export
endpoint.mlm <- function(x) {
  ret <- x |>
    endpoint.default() |>
    as.list.default()
  return(ret[-1L])
}





