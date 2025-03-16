

# proper name of estimates for various regression models

estnm <- function(x) UseMethod(generic = 'estnm')

estnm_ <- function(x) {
  if (inherits(x, what = 'error')) return('Estimate')
  # `getLink(zeroinfl)` returns multiple links
  vapply(X = x, FUN = function(i) {
    switch(EXPR = i, identity = {
      'Mean Diff.'
    }, 
    probit =,
    logit =, 
    multilogit =, # VGAM::vglm
    logistic = { # ?MASS::polr
      'Odds Ratio'
    }, log = {
      'Mean Ratio'
    }, 'Estimate')
  }, FUN.VALUE = NA_character_)
}


#' @importFrom stats family
#' @export
estnm.default <- function(x) {
  # I do not have a method to detect Cox models yet.
  # [estnm.coxph], [estnm.coxme] are written manually
  
  tryCatch(getLink(x), error = identity) |>
    estnm_()
}


#' @export
estnm.glht <- function(x) estnm(x$model)
# requires S3 generic [estnm()]; do *not* move to \pkg{multcomp.tzh}








