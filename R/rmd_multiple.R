
# multiple models
rmd_multiple_ <- function(x, xnm, ...) {
  
  n <- x |> ecip() |> length()
  
  ret <- character()
  for (i in seq_len(n)) ret <- c(
    ret, 
    '```{r}', 
    sprintf(fmt = '(ecip(%s)[[%d]]) |> as_flextable.ecip()', xnm, i),
    '```'
  ) # silly but works!!
  
  return(c(
    Sprintf(x), # S3 generic [Sprintf()]
    ret,
    '<any-text>'
  ))
  
}

#' @rdname rmd_
#' @examples
#' list(
#'   '`mlm`' = lm(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
#' ) |> render_(file = 'mlm')
#' @export rmd_.mlm
#' @export
rmd_.mlm <- rmd_multiple_

#' @rdname rmd_
#' @examples
#' library(pscl); list(
#'  '`zeroinfl`' = zeroinfl(art ~ . | 1, data = bioChemists)
#' ) |> render_(file = 'zeroinfl')
#' @export rmd_.zeroinfl
#' @export
rmd_.zeroinfl <- rmd_multiple_




