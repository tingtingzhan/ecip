
# require S3 generic [.pval()]; do *not* move to children packages!!


#' @title Convert into Multiple Univariable Structures
#' 
#' @param object see **Usage**
#' 
#' @param ... additional parameters, currently of no
#' 
#' @name as.univar
#' @aliases univar
#' @export
as.univar <- function(object, ...) UseMethod(generic = 'as.univar')


# `variables in terms`
#' @importFrom stats terms
.vterms <- function(x) {
  tmp <- x |>
    terms() |> # ?stats:::terms.terms handles exception :)
    attr(which = 'variables', exact = TRUE) |>
    as.list.default()
  tmp[-1L] # remove first element of quote(list)
}




#' @rdname as.univar
#' @param envir \link[base]{environment}, 
#' for \link[stats]{terms} and \link[stats]{formula} dispatches,
#' so that the returned univariable formula(s) are 
#' in the same \link[base]{environment} as the input  
#' 
#' @examples
#' as.univar(y ~ x1 + I(x2^2))
#' @method as.univar terms
#' @export as.univar.terms
#' @export
as.univar.terms <- function(object, envir = parent.frame(), ...) {
  v <- .vterms(object)
  lapply(v[-1], FUN = function(e) eval(call(name = '~', v[[1L]], e), envir = envir))
}


#' @rdname as.univar
#' @param formula \link[stats]{formula}
#' @importFrom stats terms.formula
#' @method as.univar formula
#' @export as.univar.formula
#' @export
as.univar.formula <- function(formula, envir = parent.frame(), ...) {
  formula |> terms.formula() |> as.univar.terms(envir = envir, ...)
}


#' @rdname as.univar
#' @examples
#' lm(mpg ~ cyl + am + hp + wt + qsec + drat + disp, data = mtc) |>
#'   as.univar()
#' @importFrom stats terms update
#' @method as.univar default
#' @export as.univar.default
#' @export
as.univar.default <- function(object, ...) {
  
  ret <- object |> 
    terms() |> 
    as.univar.terms() |>
    lapply(FUN = function(fom) update(object, formula. = fom))
  # univariable regression models
  
  fn_min_pvalue <- function(x) {
    p0 <- .pval(x) # use S3 [.pval()]
    p <- p0[!isIntercept(names(p0))] # remove intercept important!
    # missing pvalue: missing observed event ('coxph'), etc
    if (all(is.na(p))) return(1) # do not want to pick
    min(p, na.rm = TRUE)
  }
  
  attr(ret, which = 'min_pvalue') <- ret |>
    vapply(FUN = fn_min_pvalue, FUN.VALUE = NA_real_)
  class(ret) <- c(
    'univar', 
    'listof' # tzh is learning about 'listof'
  )
  return(ret)
  
}


#' @export
as.matrix.univar <- function(x, ...) {
  # !! needed when combining `univar` and `multivar`
  # !! only prints \link[flextable.tzh]{format_pval} and `@nobs`
  y <- x |> 
    lapply(FUN = function(i) {
      i |>
        ecip() |>
        intercept_rm.ecip() |>
        simple_matrix_ecip()
    })
  
  ret <- y |> 
    do.call(what = rbind)
  attr(ret, which = 'nrow') <- y |> vapply(FUN = nrow, FUN.VALUE = NA_integer_)
  return(ret)
}



#' @title Convert [univar] Object to \link[flextable]{flextable}
#' 
#' @description ..
#'  
#' @param x [univar] object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns 
#' Function [as_flextable.univar()] returns a \link[flextable]{flextable}.
#'  
#' @importFrom flextable as_flextable
#' @export as_flextable.univar
#' @export
as_flextable.univar <- function(x, ...) {
  y <- x |> 
    as.matrix.univar()
  
  y |> 
    as_flextable.array(
      row.title = deparse1(endpoint(x[[1L]])),
      hline_i = y |> attr(which = 'nrow', exact = TRUE) |> cumsum()
    )
}



#' @export
print.univar <- function(x, ...) {
  x |> as_flextable.univar() |> print()
}










# base::subset
#' @title subset.univar
#' 
#' @param x [as.univar] object
#' 
#' @param subset ..
#' 
#' @param ... ..
#' 
#' @export subset.univar
#' @export
subset.univar <- function(x, subset, ...) {
  subset <- substitute(subset)
  v_sub <- all.vars(subset)
  if (!all(v_sub %in% c('min_pvalue'))) stop('criterion must be set on `min_pvalue`, for now')
  min_pvalue <- attr(x, which = 'min_pvalue', exact = TRUE)
  ret <- x[eval(subset)]
  attr(ret, which = 'p_thres') <- subset[[3L]]
  return(ret)
}



#' @export
`[.univar` <- function(x, i) {
  ret <- unclass(x)[i]
  attr(ret, which = 'min_pvalue') <- attr(x, which = 'min_pvalue', exact = TRUE)[i]
  class(ret) <- class(x) # otherwise class info dropped
  return(ret)
}

