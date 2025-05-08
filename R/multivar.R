
#' @title as.multivar
#' 
#' @param x ..
#' 
#' @param ... parameters of [subset.univar()]
#' 
#' @examples
#' # see ?md_
#' @importFrom MASS.tzh stepAIC_complete
#' @importFrom stats update
#' @aliases multivar
#' @export
as.multivar <- function(x, ...) {
  
  if (!inherits(x, what = 'univar')) stop()
  
  lhs <- vterms(x[[1L]])[[1L]] # do not bother to check `lhs` being same accross `x`
  
  x1 <- x |> 
    subset.univar(...)
  
  if (length(x1)) {
    #rhs <- x1 |>
    #  vapply(FUN = \(i) {
    #    (vterms(i)[[2L]]) |> deparse1()
    #  }, FUN.VALUE = '') |>
    #  paste(collapse = ' + ') |>
    #  str2lang() # only good for non-random effects
    trm <- x1 |> lapply(FUN = vterms)
    group <- trm |> lapply(FUN = attr, which = 'group', exact = TRUE) |> unique()
    if (length(group) != 1L) stop('should not happen')
    fix. <- trm |>
      lapply(FUN = '[[', 2L) |>
      Reduce(f = \(e1, e2) call(name = '+', e1, e2))
    rhs <- if (!length(group[[1L]])) fix. else {
      c(list(fix.), group[[1L]]) |>
        Reduce(f = \(e1, e2) call(name = '+', e1, e2))
    }
  } else {
    rhs <- 1 # only good for non-random effects!!!
  }
  
  ret <- x[[1L]] |> 
    update(formula. = call(name = '~', lhs, rhs) |> eval()) |>
    stepAIC_complete()
  attr(ret, which = 'univar') <- x
  attr(ret, which = 'p_thres') <- attr(x1, which = 'p_thres', exact = TRUE)
  class(ret) <- c('multivar', class(ret))
  return(ret)
  
}



#' @title as_flextable.multivar
#' 
#' @description ..
#'  
#' @param x [multivar] object
#' 
#' @param ... additional parameters, currently of no use
#'  
#' @importFrom flextable as_flextable color
#' @importFrom scales.tzh label_pvalue_sym
#' @export as_flextable.multivar
#' @export
as_flextable.multivar <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  u <- x |> 
    attr(which = 'univar', exact = TRUE) |>
    as.matrix.univar()
  
  names(x)[[1L]] <- paste(
    names(x)[[1L]], 
    x |> 
      attr(which = 'p_thres', exact = TRUE) |> 
      label_pvalue_sym(add_p = TRUE)()
  )
  
  m <- x |>
    as.matrix.stepAIC() |>
    intercept_rm.matrix()
  
  ret <- cbind(u, 
               array('\U1f6ab', dim = c(nrow(u), ncol(m)), dimnames = list(NULL, colnames(m))))
  id <- match(rownames(m), table = rownames(u))
  if (anyNA(id)) stop('should not happen')
  ret[id, seq_len(ncol(m))+1L] <- m
  colnames(ret)[1L] <- paste('(Univariable)', colnames(ret)[1L], sep = '\n')
  
  ret |>
    as_flextable.matrix(
      row.title = ecip(x[[length(x)]])@endpoint,
      hline_i = u |> attr(which = 'nrow', exact = TRUE) |> cumsum()
    ) |>
    color(
      j = 2:3, # univariable column, initial multivariable column
      color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      '\u274c: predictor(s) removed by stepwise algorithm.',
      '\U1f6ab: predictor(s) not considered in the model.'
    ))

}



#' @export
print.multivar <- function(x, ...) {
  x |> as_flextable.multivar() |> print()
}






#' @export
endpoint.multivar <- function(x) (x[[length(x)]]) |> endpoint()

#' @export
nobsText.multivar <- function(x) (x[[length(x)]]) |> nobsText()

#' @export
desc_.multivar <- function(x) (x[[length(x)]]) |> desc_()





#' @title Model Description of [multivar] Object
#' 
#' @description ..
#' 
#' @param x a [multivar] object
#' 
#' @importFrom stats formula
#' @importFrom rmd.tzh pkg_text
#' @export Sprintf.multivar
#' @export
Sprintf.multivar <- function(x) {
  
  u <- x |> attr(which = 'univar', exact = TRUE)
  v <- vapply(u, FUN = \(i) deparse1(vterms(i)[[2L]]), FUN.VALUE = '')
  
  str1 <- sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by first fitting univariable *%s* models due to the limited sample size, denegerated experimental design and/or substantial missingness across the predictors, using %s.',
    x |> endpoint() |> deparse1(),
    paste0('`', v, '`', collapse = ', '),
    nobsText(x),
    desc_(x),
    u[[1L]] |> pkg_text()
  )
  
  str2 <- sprintf(
    fmt = 'Next, univariable predictor(s) with $p$-value<%.2f are considered for the multivariable model. Lastly, a backward stepwise variable selection by [Akaike information criterion (AIC)](https://en.wikipedia.org/wiki/Akaike_information_criterion) is performed using <u>**`R`**</u> package <u>**`MASS`**</u>.',
    x |> attr(which = 'p_thres', exact = TRUE)
  )
  
  #str3 <- if (length(force <- attr(x, which = 'force', exact = TRUE))) {
  #  sprintf(fmt = 'Predictor(s) %s are retained in the multivariable model regardless.', 
  #          paste0('`', force, '`', collapse = ', '))
  #}
  #return(paste(str1, str2, str3))
  
  return(paste(str1, str2))
  
}




