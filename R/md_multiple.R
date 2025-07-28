
# multiple models
md_multiple_ <- function(x, xnm, ...) {
  
  n <- x |> ecip() |> length()
  
  ret <- character()
  for (i in seq_len(n)) ret <- c(
    ret, 
    '```{r}', 
    '#| echo: false', 
    sprintf(fmt = '(ecip(%s)[[%d]]) |> as_flextable.ecip()', xnm, i),
    '```'
  ) # silly but works!!
  
  z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
    new(Class = 'md_lines')
  
  z2 <- ret |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
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
#' library(pscl.tzh); list(
#'  '`zeroinfl`' = zeroinfl(art ~ . | 1, data = bioChemists)
#' ) |> render_(file = 'zeroinfl')
#' @export md_.zeroinfl
#' @export
md_.zeroinfl <- md_multiple_




