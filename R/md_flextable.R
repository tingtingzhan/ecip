
#' @importFrom methods new
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
#' @export md_.aov
#' @export
md_.aov <- md_flextable_

#' @rdname md_flextable
#' @examples
#' list(
#'   '`TukeyHSD`' = aov(breaks ~ wool + tension, data = warpbreaks) |> 
#'     TukeyHSD(which = 'tension', ordered = TRUE)
#' ) |> render_(file = 'aov_etc')
#' @export md_.TukeyHSD
#' @export
md_.TukeyHSD <- md_flextable_


#' @rdname md_flextable
#' @export md_.summary.matchit
#' @export
md_.summary.matchit <- md_flextable_ 



# below: small variations on [md_flextable_()]

#' @rdname md_flextable
#' @examples
#' library(multcomp); list(
#'  '`glht` via `aov`' = aov(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett')),
#'  '`glht` via `lm`, single `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey')),
#'  '`glht` via `lm`, multiple `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett'))
#' ) |> render_(file = 'glht')
#' 
#' @export md_.glht
#' @export
md_.glht <- function(x, xnm, ...) {
  if (!is.character(xnm)) xnm <- deparse1(xnm)
  return(c(
    md_(x$model, xnm = paste0(xnm, '$model'), ...),
    md_flextable_(x, xnm = xnm, ...)
  ))
}



#' @rdname md_flextable
#' @examples
#' library(DanielBiostatistics10th); list(
#'   '`binTab`' = binTab(array(c(7L, 3L, 8L, 6L), dim = c(2,2)))
#' ) |> render_(file = 'binTab')
#' @export md_.binTab
#' @export
md_.binTab <- function(x, xnm, ...) {
  
  z1 <- md_flextable_(x, xnm = xnm)
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    '#| comment:', 
    paste0('summary.binTab(', xnm, ')'), # how to put in `prevalence` here??
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}



#' @rdname md_flextable
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render_(file = 'DemographicTable')
#' @export md_.DemographicTable
#' @export
md_.DemographicTable <- function(x, xnm, font.size = 9, ...) {
  md_flextable_(x, xnm = xnm, font.size = font.size, ...)
}



#' @rdname md_flextable
#' @examples
#' library(MatchIt)
#' m = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde)
#' list(
#'  '`matchit1`' = m,
#'  '`matchit2`' = m |> summary(addlvariables = 're78')
#' ) |> render_(file = 'matchit')
#' @export md_.matchit
#' @export
md_.matchit <- function(x, xnm, ...) {
  md_.summary.matchit(
    x = summary(x), # ?MatchIt:::summary.matchit
    xnm = sprintf(fmt = 'summary(%s)', xnm),
    ...)
}




#' @rdname md_flextable
#' @export md_.stepAIC
#' @export
md_.stepAIC <- md_flextable_




#' @rdname md_flextable
#' @export md_.backwardCriterion
#' @export
md_.backwardCriterion <- md_flextable_

