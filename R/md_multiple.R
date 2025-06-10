
# multiple models
md_multiple_ <- function(x, xnm, ...) {
  
  n <- x |> ecip() |> length()
  
  ret <- character()
  for (i in seq_len(n)) ret <- c(
    ret, 
    '```{r}', 
    sprintf(fmt = '(ecip(%s)[[%d]]) |> as_flextable.ecip()', xnm, i),
    '```'
  ) # silly but works!!
  
  txt <- Sprintf(x) # S3 generic [Sprintf()]
  ret <- c(
    txt,
    ret
  )
  bib <- txt |> attr(which = 'bibentry', exact = TRUE)
  if (length(bib)) attr(ret, which = 'bibentry') <- bib
  return(ret)
}

#' @rdname md_
#' @examples
#' list(
#'   '`mlm`' = lm(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
#' ) |> render_(file = 'mlm')
#' @export md_.mlm
#' @export
md_.mlm <- md_multiple_

#' @rdname md_
#' @examples
#' library(pscl); list(
#'  '`zeroinfl`' = zeroinfl(art ~ . | 1, data = bioChemists)
#' ) |> render_(file = 'zeroinfl')
#' @export md_.zeroinfl
#' @export
md_.zeroinfl <- md_multiple_




