
#' @title R Markdown Lines for via Multiple \link[ecip]{ecip}s 
#' 
#' @param x an R object convertible to multiple \link[ecip]{ecip}s
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param ... ..
#' 
#' @details
#' \itemize{
#' \item{`'mlm'` object, returned object from multiple endpoints in \link[stats]{lm};}
#' \item{`?pscl::zeroinfl`}
#' }
#' 
#' 
#' @keywords internal
#' @export
md_ecips <- function(x, xnm, ...) {
  
  z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'tmp = ecip(%s)'),
    x |> 
      ecip() |> 
      seq_along() |>
      vapply(FUN = sprintf, fmt = '(tmp[[%d]]) |> as_flextable.ecip()', FUN.VALUE = NA_character_),
    'rm(tmp)',
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}

#' @rdname md_
#' @examples
#' list(
#'   '`mlm`' = lm(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
#' ) |> rmd.tzh::render_(file = 'mlm')
#' @export md_.mlm
#' @export
md_.mlm <- md_ecips

