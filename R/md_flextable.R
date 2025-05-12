

md_flextable_ <- function(x, xnm, font.size, ...) {
  return(c(
    Sprintf(x), # S3 generic [Sprintf()]
    '```{r}', 
    if (!missing(font.size)) font.size |> sprintf(fmt = 'set_flextable_defaults(font.size = %.1f)'),
    sprintf(fmt = '(%s) |> as_flextable()', xnm),
    if (!missing(font.size)) 'init_flextable_defaults()',
    '```'
  ))
}

#' @name md_
#' @export md_.listof
#' @export
md_.listof <- md_flextable_

#' @rdname md_
#' @examples
#' library(lme4)
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

#' @rdname md_
#' @export md_.aov
#' @export
md_.aov <- md_flextable_

#' @rdname md_
#' @examples
#' list(
#'   '`TukeyHSD`' = aov(breaks ~ wool + tension, data = warpbreaks) |> 
#'     TukeyHSD(which = 'tension', ordered = TRUE)
#' ) |> render_(file = 'aov_etc')
#' @export md_.TukeyHSD
#' @export
md_.TukeyHSD <- md_flextable_


#' @rdname md_
#' @export md_.summary.matchit
#' @export
md_.summary.matchit <- md_flextable_ 



# below: small variations on [md_flextable_()]

#' @rdname md_
#' @export md_.glht
#' @export
md_.glht <- function(x, xnm, ...) {
  if (!is.character(xnm)) xnm <- deparse1(xnm)
  return(c(
    md_(x$model, xnm = paste0(xnm, '$model'), ...),
    md_flextable_(x, xnm = xnm, ...)
  ))
}

#' @rdname md_
#' @examples
#' library(DanielBiostatistics10th); list(
#'   '`binTab`' = binTab(array(c(7L, 3L, 8L, 6L), dim = c(2,2)))
#' ) |> render_(file = 'binTab')
#' @export md_.binTab
#' @export
md_.binTab <- function(x, xnm, ...) {
  return(c(
    md_flextable_(x, xnm = xnm),
    '```{r comment = NA}', 
    paste0('summary.binTab(', xnm, ')'), # how to put in `prevalence` here??
    '```'
  ))
}

#' @rdname md_
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render_(file = 'DemographicTable')
#' @export md_.DemographicTable
#' @export
md_.DemographicTable <- function(x, xnm, font.size = 9, ...) {
  md_flextable_(x, xnm = xnm, font.size = font.size, ...)
}



#' @rdname md_
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


