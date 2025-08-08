
#' @title Coefficient Estimates, Confidence Intervals and \eqn{p}-Values
#' 
#' @description ..
#' 
#' @slot model original regression model
#' 
#' @slot coef \link[base]{double} \link[base]{vector}
#' 
#' @slot p.value \link[base]{double} \link[base]{vector}
#' 
#' @slot conf.int \link[base]{ncol}-`2L` \link[base]{double} \link[base]{matrix}, with
#' an \link[base]{attributes} of `attr(.,'conf.level')`, 
#' inspired by the returned value of functions such as \link[stats]{t.test}.
#' 
#' @slot endpoint ..
#' 
#' @slot nobs ..
#' 
#' @slot exp \link[base]{logical} scalar
#' 
#' @slot estnm ..
#' 
#' @slot note ..
#' 
#' @name ecip
#' @aliases ecip-class
#' @export
setClass(Class = 'ecip', slots = c(
  model = 'ANY',
  coef = 'numeric',
  p.value = 'numeric',
  conf.int = 'matrix',
  exp = 'logical',
  endpoint = 'character',
  nobs = 'character',
  estnm = 'character',
  note = 'character' # vector for each estimated values
  #nrows = 'integer' # user friendly output for 'glht' object; .Defunct!!!
))


setValidity(Class = 'ecip', method = function(object) {
  
  p <- object@p.value
  cf <- object@coef
  ci <- object@conf.int
  
  if (length(p)) { # having \eqn{p}-value
    if (!identical(names(p), names(cf))) stop('[.pval.*] must return the same set of parameters as [coef_.*]')
  }
  
  if (!identical(rownames(ci), names(cf))) stop('[confint_.*] must return the same set of parameters as [coef_.*]')
  
  if (length(object@estnm) != 1L) stop('estnm must be of len-1')
  if ((nnt <- length(object@note)) > 1L && (nnt != length(cf))) stop('length of note does not match!')
  if (length(object@nobs) != 1L) stop('`cip` object must have len-1 nobs_txt')
  
})


#' @rdname ecip
#' @param model see slot `@model`
#' 
#' @export
ecip <- function(model) {
  
  if (inherits(model, what = 'listof')) {
    return(model |>
      lapply(FUN = ecip) |>
      structure(class = 'listof'))
  }
  
  cf <- coef_(model)
  p <- .pval(model)
  ci <- confint_(model)
  edp <- endpoint(model)
  expo <- expcoef(model)
  estname <- estnm(model)
  
  if (is.list(cf)) {
    n <- length(cf)
    if (!is.list(p) || length(p) != n) stop()
    if (!is.list(ci) || length(ci) != n) stop()
    if (!is.list(edp) || length(edp) != n) stop()
    if (length(expo) == 1L) expo <- replicate(n = n, expr = expo)
    if (length(estname) == 1L) estname <- replicate(n = n, expr = estname)
    ret <- .mapply(FUN = new, dots = list(
      coef = cf, 
      p.value = p, 
      conf.int = ci, 
      endpoint = edp |> lapply(FUN = deparse1), 
      exp = expo, 
      estnm = estname
    ), MoreArgs = list(
      Class = 'ecip', model = model, nobs = nobsText(model), note = note_(model)
    ))
    class(ret) <- 'listof'
    return(ret)
  }

  new(
    Class = 'ecip',
    model = model,
    coef = cf,
    p.value = p,
    conf.int = ci,
    endpoint = edp |> deparse1(),
    nobs = nobsText(model),
    exp = expo,
    estnm = estname,
    note = note_(model)
  )
  
}  


#' @importFrom methods callNextMethod
setMethod(f = initialize, signature = 'ecip', definition = function(.Object, ...) {
  
  x <- callNextMethod(.Object, ...)
  
  nm <- names(x@coef)
  new_nm <- if (!inherits(x@model, c(
    'glht',
    'lcmm', 'cronbachAlpha', 'Kappa'
  ))) { # add more classes
    refineTerm(nm, model = x@model) 
  } else refineTerm(nm)
  names(x@coef) <- new_nm 
  # dont bother with `names(x@p.value)` and `rownames(x@conf.int)`
  # um.. obviously ?methods::setValidity happens *before* ?methods::initialize

  return(x)
  
})





#' @title Convert \linkS4class{ecip} into \link[base]{matrix}
#' 
#' @description ..
#' 
#' @param x \linkS4class{ecip} object
#' 
#' @param fmt ..
#' 
#' @param type `'full'` (default), `'ncol1'`, `'p_samplesize'` `'p_only'`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @importFrom rmd.tzh label_pvalue_sym
#' @method as.matrix ecip
#' @export as.matrix.ecip
#' @export
as.matrix.ecip <- function(
    x, 
    fmt = '%.2f', 
    type = c('full', 'ncol1', 'p_samplesize', 'p_only'),
    ...
) {
  
  type <- match.arg(type)
  
  p <- x@p.value |> label_pvalue_sym(add_p = (type %in% c('ncol1', 'p_samplesize', 'p_only')))()
  
  if (type == 'p_only') {
    if (!length(p)) stop('not applicable to `p_only`')
    ret <- p
    dim(ret) <- c(length(p), 1L)
    dimnames(ret) <- list(names(x@coef), paste(x@estnm, x@nobs, sep = '\n'))
    return(ret)
  }
  
  if (type == 'p_samplesize') {
    if (!length(p)) stop('not applicable to `p_samplesize`')
    ret <- paste(p, x@nobs, sep = '\n')
    dim(ret) <- c(length(p), 1L)
    dimnames(ret) <- list(names(x@coef), x@estnm)
    return(ret)
  }
  
  cf <- x@coef
  cf_ok <- !is.na(cf)
  
  cf_arrow <- ifelse(cf < 0, yes = '\u2193', no = '\u2191') # before exp() !!!
  cf_arrow[!cf_ok] <- '' # important! for those without a 'reference'-estimate
  
  if (x@exp) cf <- exp(cf)
  cf_ <- sprintf(fmt = fmt, cf)
  cf_[!cf_ok] <- ''
  
  ci <- x@conf.int
  if (x@exp) ci <- exp(ci)
  level <- attr(ci, which = 'conf.level', exact = TRUE)
  if (!length(level)) stop('[confint_.*] return has no conf.level attribute')
  ci_ <- paste0('(', format.confint(ci, fmt = fmt, ...), ')', cf_arrow)

  cf_ci_ <- paste(cf_, ci_, sep = ' ')
  cf_ci_nm <- sprintf(fmt = '%s (%.f%% CI)', x@estnm, 1e2*level)
  
  switch(type, ncol1 = {
    ret <- if (!length(p)) {
      cf_ci_
    } else if (length(x@note)) {
      paste(cf_ci_, paste(p, x@note, sep = '; '), sep = '\n')
    } else paste(cf_ci_, p, sep = '\n')
    dim(ret) <- c(length(ret), 1L)
    dimnames(ret) <- list(names(cf), paste(cf_ci_nm, x@nobs, sep = '\n'))
  }, full = {
    if (length(p)) {
      ret <- cbind(cf_ci_, p)
      dimnames(ret) <- list(names(cf), c(cf_ci_nm, 'Signif.'))
    } else {
      ret <- cf_ci_
      dim(ret) <- c(length(cf_ci_), 1L)
      dimnames(ret) <- list(names(cf), c(cf_ci_nm))
    }
    if (length(x@note)) ret <- cbind(ret, Note = x@note)
  })
  
  return(ret)
  
  #if (length(x@nrows)) attr(ret, which = 'nrows') <- x@nrows # to be defunct
}








#' @title Convert \linkS4class{ecip} into \link[flextable]{flextable}
#' 
#' @description ..
#' 
#' @param x \linkS4class{ecip} object
#' 
#' @param row.title ..
#' 
#' @param ... potential parameters of functions [as.matrix.ecip()] 
#' and \link[flextable.tzh]{as_flextable.matrix}
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.ecip
#' @export
as_flextable.ecip <- function(
    x, 
    row.title = sprintf(fmt = '%s: %s', x@endpoint, x@nobs),
    ...
) {
  x |>
    as.matrix.ecip(...) |>
    as_flextable.matrix(row.title = row.title, ...)
}








#' @title Show \linkS4class{ecip} Object
#' 
#' @description ..
#' 
#' @param object \linkS4class{ecip} object
#' 
#' @importFrom methods show signature
#' @export
setMethod(f = show, signature = signature(object = 'ecip'), definition = function(object) {
  object |> 
    as_flextable.ecip() |>
    print() # ?flextable:::print.flextable
})








#' @export
`[.ecip` <- function(x, i, ...) {# j-index always TRUE
  
  if (!length(i)) return(invisible())
  if (isTRUE(i)) return(x)
  
  newx <- x
  newx@model <- NULL
  newx@coef <- x@coef[i]
  newx@conf.int <- x@conf.int[i, , drop = FALSE]
  attr(newx@conf.int, which = 'conf.level') <- attr(x@conf.int, which = 'conf.level', exact = TRUE)
  if (length(x@p.value)) newx@p.value <- x@p.value[i]
  if (length(x@note)) newx@note <- x@note[i]
  
  # return(new(Class = 'ecip', newx)) # I don't want to onset setValidity :)
  return(newx)
  
}





#' @title [isIntercept]
#' 
#' @param x ..
#' 
#' @export
isIntercept <- function(x) { 
  if (is.null(x)) return(FALSE)
  if (!is.character(x)) stop('Input must be character (e.g. coefficients names)')
  if (anyNA(x)) stop('do not allow NA_character_ in names!')
  if (!(nx <- length(x)) || nx == 1L) return(FALSE) # do not trim for non-named object, or intercept-only model
  #startsWith(x, prefix = '(Intercept)')
  grepl(pattern = '\\(Intercept\\)', x = x)
}


#' @title [intercept_rm]
#' 
#' @param x ..
#' 
#' @keywords intercept_rm
#' @name intercept_rm
#' @export
intercept_rm <- function(x) UseMethod(generic = 'intercept_rm')

#' @rdname intercept_rm
#' @export intercept_rm.ecip
#' @export
intercept_rm.ecip <- function(x) {
  # my old [trim_cibeta]
  x[!isIntercept(names(x@coef)), ] # `[.ecip`
}

#' @rdname intercept_rm
#' @export intercept_rm.matrix
#' @export
intercept_rm.matrix <- function(x) {
  x[!isIntercept(rownames(x)), , drop = FALSE] # `[.ecip`
}



