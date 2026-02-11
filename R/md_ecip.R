
#' @title R Markdown Lines via \linkS4class{ecip}
#' 
#' @param x an R object convertible to one \linkS4class{ecip}
#' 
#' @param xnm,... ..
#' 
#' @keywords internal
#' @importClassesFrom fastmd md_lines
#' @export 
md_ecip <- function(x, xnm, ...) {
  
  z1 <- .md_reg(x)
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    '#| warning: false', # all kinds of warnings from [ecip()]
    sprintf(fmt = '(%s) |> ecip() |> as_flextable.ecip()', xnm),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?fastmd::c.md_lines
  
}



#' @title R Markdown Lines for \link[stats]{lm} Model
#' 
#' @param x a regression model returned from function \link[stats]{lm} or \link[stats]{glm}
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param ... ..
#' 
#' @examples
#' list(
#'  '`lm`' = lm(Sepal.Length ~ Species, data = iris)
#' ) |> fastmd::render_(file = 'lm')
#' 
#' @name md_lm
#' @keywords internal
#' @importFrom fastmd md_
#' @export md_.lm
#' @export
md_.lm <- md_ecip


