

# @returns 'logical' value, 
# @details \code{expcoef} indicating whether the parameter estimates and confidence intervals 
# should be taken exponential *manually*.

expcoef <- function(x) UseMethod(generic = 'expcoef')

#' @export
expcoef.default <- function(x) {
  # some model has no link, e.g., ?survival::coxph, then we need to write individual [expcoef.*]
  x |> estnm() |> grepl(pattern = 'Ratio')
}

#' @export
expcoef.glht <- function(x) expcoef(x$model)
# requires S3 generic [expcoef()]; do *not* move to \pkg{multcomp.tzh}


