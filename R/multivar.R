
#' @title as.multivar
#' 
#' @param x ..
#' 
#' @param ... parameters of [subset.univar()]
#' 
#' @examples
#' lm(mpg ~ cyl + am + vs + hp + wt + qsec + drat + disp, data = mtc) |>
#'  as.univar() |>
#'  as.multivar(subset = min_pvalue < .1)
#' @importFrom stats update
#' @aliases multivar
#' @export
as.multivar <- function(x, ...) {
  
  if (!inherits(x, what = 'univar')) stop()
  
  lhs <- .vterms(x[[1L]])[[1L]] # do not bother to check `lhs` being same accross `x`
  
  x1 <- x |> 
    subset.univar(...)
  
  if (length(x1)) {
    rhs <- x1 |>
      vapply(FUN = function(i) {
        (.vterms(i)[[2L]]) |> deparse1()
      }, FUN.VALUE = '') |>
      paste(collapse = ' + ') |>
      str2lang()
  } else rhs <- 1 
  
  ret <- x[[1L]] |> 
    update(formula. = eval(call(name = '~', lhs, rhs))) |>
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
#' @importFrom flextable.tzh format_pval
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
    x |> attr(which = 'p_thres', exact = TRUE) |> format_pval(add_p = TRUE)
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
    as_flextable.array(
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
#' @export Sprintf.multivar
#' @export
Sprintf.multivar <- function(x) {
  
  u <- x |> attr(which = 'univar', exact = TRUE)
  v <- vapply(u, FUN = function(i) deparse1(.vterms(i)[[2L]]), FUN.VALUE = '')
  
  str1 <- sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by first fitting univariable *%s* models due to the limited sample size, denegerated experimental design and/or substantial missingness across the predictors, using %s.',
    x |> endpoint() |> deparse1(),
    paste0('`', v, '`', collapse = ', '),
    nobsText(x),
    desc_(x),
    '<u>**`R`**</u>' # pkgText(x) # this is tzh's \pkg{tzh}
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




