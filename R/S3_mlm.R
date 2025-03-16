

# return from stats::lm

# note that we have ?stats:::vcov.mlm inside ?stats::confint.lm

#' @export
coef_.mlm <- function(x) {
  # otherwise dispatch to ?stats:::coef.default
  cf <- x$coefficients # endpoints on columns
  ret <- lapply(seq_len(dim(cf)[2L]), FUN = function(i) cf[,i])
  names(ret) <- dimnames(x$coefficients)[[2L]]
  return(ret)
}

#' @importFrom stats confint.lm
#' @export
confint_.mlm <- function(x, level = .95, ...) {
  # otherwise dispatch to ?stats::confint.lm
  ci <- confint.lm(x, level = level, ...)
  rnm <- dimnames(ci)[[1L]]
  edp <- dimnames(x$coefficients)[[2L]]
  ret <- lapply(paste0(edp, ':'), FUN = function(i) {
    out <- ci[startsWith(rnm, prefix = i), , drop = FALSE]
    rownames(out) <- gsub(i, replacement = '', x = rownames(out))
    attr(out, which = 'conf.level') <- level
    return(out)
  })
  names(ret) <- edp
  return(ret)
}


#' @export
endpoint.mlm <- function(x) {
  ret <- x |>
    endpoint.default() |>
    as.list.default()
  return(ret[-1L])
}


#' @export
desc_.mlm <- function(x) 'multivariate linear regression' # ?stats::lm with endpoint `cbind(., .)`



