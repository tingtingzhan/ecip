

#' @title Proper Name of Estimates
#' 
#' @description
#' Proper name of estimates for various regression models
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [estnm()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @name estnm
#' @export
estnm <- function(x) UseMethod(generic = 'estnm')



#' @rdname estnm
#' @importFrom stats family
#' @export estnm.default
#' @export
estnm.default <- function(x) {

  lk <- tryCatch(getLink(x), error = identity)
  if (inherits(lk, what = 'error')) return('Estimate')
  
  lk |>
    vapply(FUN = \(i) {
      switch(EXPR = i, identity = {
        # 'Mean Diff.' # this will be confusing for idiots
        'Estimate'
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


#' @rdname estnm
#' @export estnm.glht
#' @export
estnm.glht <- function(x) estnm(x$model)
# requires S3 generic [estnm()]; do *not* move to \pkg{multcomp.tzh}




#' @title Taking Exponential of Coefficients
#' 
#' @description
#' To determine whether to taking exponential of coefficients estimates and confidence intervals.
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [expcoef()] returns a \link[base]{logical} scalar.
#' 
#' @keywords internal
#' @name expcoef
#' @export
expcoef <- function(x) UseMethod(generic = 'expcoef')

#' @rdname expcoef
#' @export expcoef.default
#' @export
expcoef.default <- function(x) {
  x |> estnm() |> grepl(pattern = 'Ratio')
}

#' @rdname expcoef
#' @export expcoef.glht
#' @export
expcoef.glht <- function(x) expcoef(x$model)
# requires S3 generic [expcoef()]; do *not* move to \pkg{multcomp.tzh}







