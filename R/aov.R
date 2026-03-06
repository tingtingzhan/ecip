
#' @export
.pval.anova <- function(x) {
  ret <- x[, 'Pr(>F)']
  names(ret) <- rownames(x) # nrow-1 drops rownames
  return(ret)
}


#' @method .pval summary.aov
#' @export
.pval.summary.aov <- function(x) {
  x |>
    lapply(FUN = .pval.anova) |>
    unlist()
}

