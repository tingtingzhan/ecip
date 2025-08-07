

# library(logistf); library(rmd.tzh); list(
#  '`logistf`' = logistf(case ~ age+oc+vic+vicl+vis+dia, data = sex2)
# ) |> render_(file = 'logistf')
# # I will finalize \pkg{logistf.tzh} when I have time :)
# # this example works fine when logistf::logistf was supported in \pkg{tzh}

#' @title R Markdown Lines via \link[ecip]{ecip}
#' 
#' @param x,xnm,... ..
#' 
#' @keywords internal
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @export 
md_ecip <- function(x, xnm, ...) {
  
  z1 <- Sprintf(x) # S3 generic [Sprintf()]
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    sprintf(fmt = '(%s) |> ecip() |> as_flextable.ecip()', xnm),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}







#' @title \pkg{rmarkdown} Lines based on [ecip()]
#' 
#' @param x .. 
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(rmd.tzh)
#' 
#' list(
#'  '`lm`' = lm(Sepal.Length ~ Species, data = iris)
#' ) |> render_(file = 'lm')
#' 
#' library(MASS); list(
#'  '`rlm`' = rlm(stack.loss ~ ., data = stackloss)
#' ) |> render_(file = 'rlm')
#' 
#' @name md_
#' @importFrom rmd.tzh md_
#' @export md_.lm
#' @export
md_.lm <- md_ecip



#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.rlm
#' @export
md_.rlm <- md_ecip

