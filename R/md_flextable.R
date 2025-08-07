

#' @title R Markdown by \link[flextable]{flextable}
#' 
#' @param x,xnm,font.size,... ..
#' 
#' @keywords internal
#' @importFrom methods new
#' @export
md_flextable_ <- function(x, xnm, font.size, ...) {
  
  z1 <- Sprintf(x) |> # S3 generic [Sprintf()]
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    if (!missing(font.size)) font.size |> sprintf(fmt = 'set_flextable_defaults(font.size = %.1f)'),
    sprintf(fmt = '(%s) |> as_flextable()', xnm),
    if (!missing(font.size)) 'init_flextable_defaults()',
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}



#' @title \pkg{rmarkdown} Lines based on \link[flextable]{flextable}
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param font.size see function \link[flextable]{set_flextable_defaults}
#' 
#' @examples
#' library(rmd.tzh)
#' 
#' @keywords internal
#' @name md_flextable
#' @export md_.listof
#' @export
md_.listof <- md_flextable_

#' @rdname md_flextable
#' @examples
#' library(lme4.tzh)
#' library(HSAUR3)
#' library(ordinal)
#' m1 = lm(mpg ~ cyl + am + hp + wt + qsec + drat + disp, data = mtc)
#' m2 = glmer(outcome ~ treatment + visit + (1|patientID), data = toenail,
#'   family = binomial, nAGQ = 20)
#' m3 = clmm(SURENESS ~ PROD + SOUPTYPE + (1|RESP) + (1|RESP:PROD), data = soup,
#'  link = 'probit', threshold = 'equidistant')
#'   
#' list(
#'  'multivar, `lm`' = m1 |> as.univar() |> as.multivar(subset = min_pvalue < .1),
#'  'multivar, `merMod`' = m2 |> as.univar() |> as.multivar(subset = min_pvalue < .1)# ,
#'  # 'multivar, `clmm`' = m3 |> as.univar() |> as.multivar(subset = min_pvalue < .1)# still bug
#' ) |> render_(file = 'multivar')
#' @export md_.multivar
#' @export
md_.multivar <- md_flextable_


#' @rdname md_flextable
#' @export md_.stepAIC
#' @export
md_.stepAIC <- md_flextable_


#' @rdname md_flextable
#' @export md_.backwardCriterion
#' @export
md_.backwardCriterion <- md_flextable_

