
#' @export
desc_.lm <- function(x) 'ordinary least squares'

#' @method .pval summary.lm
#' @export
.pval.summary.lm <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|t|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}



#' @export
desc_.glm <- function(x) 'generalized linear'


#' @method .pval summary.glm
#' @export
.pval.summary.glm <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}