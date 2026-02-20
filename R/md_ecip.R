
#' @title R Markdown Lines via \linkS4class{ecip}
#' 
#' @param x an R object convertible to \linkS4class{ecip} or `eciplist`
#' 
#' @param xnm,... ..
#' 
#' @examples
#' list(
#'  '`lm`' = lm(Sepal.Length ~ Species, data = iris),
#'  '`mlm`' = lm(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
#' ) |> fastmd::render2html()
#' @keywords internal
#' @importClassesFrom fastmd md_lines
#' @importFrom fastmd md_flextable_
#' @export 
md_ecip <- function(x, xnm, ...) {
  
  z1 <- md_regression_(x)
  
  z2 <- xnm |> 
    sprintf(fmt = '(%s) |> ecip()') |> 
    md_flextable_(xnm = _, ...)
    
  c(z1, z2) # ?fastmd::c.md_lines
  
}




#' @importFrom fastmd md_
#' @export
md_.lm <- md_ecip



#' @importFrom fastmd md_
#' @export
md_.mlm <- md_ecip

