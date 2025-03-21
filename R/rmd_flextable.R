

rmd_flextable_ <- function(x, xnm, font.size, ...) {
  return(c(
    Sprintf(x), # S3 generic [Sprintf()]
    '```{r}', 
    if (!missing(font.size)) sprintf(fmt = 'flextable::set_flextable_defaults(font.size = %.1f)', font.size),
    sprintf(fmt = '(%s) |> as_flextable()', xnm),
    if (!missing(font.size)) 'flextable::init_flextable_defaults()',
    '```', 
    '<any-text>'))
}

#' @name rmd_
#' @export rmd_.listof
#' @export
rmd_.listof <- rmd_flextable_

#' @rdname rmd_
#' @examples
#' list(
#'   multivar = lm(mpg ~ cyl + am + hp + wt + qsec + drat + disp, data = mtc) |>
#'      as.univar() |>
#'      as.multivar(subset = min_pvalue < .1)
#' ) |> render_(file = 'multivar')
#' @export rmd_.multivar
#' @export
rmd_.multivar <- rmd_flextable_

#' @rdname rmd_
#' @export rmd_.stepAIC
#' @export
rmd_.stepAIC <- rmd_flextable_

#' @rdname rmd_
#' @export rmd_.aov
#' @export
rmd_.aov <- rmd_flextable_

#' @rdname rmd_
#' @examples
#' list(
#'   '`TukeyHSD`' = aov(breaks ~ wool + tension, data = warpbreaks) |> 
#'     TukeyHSD(which = 'tension', ordered = TRUE)
#' ) |> render_(file = 'aov_etc')
#' @export rmd_.TukeyHSD
#' @export
rmd_.TukeyHSD <- rmd_flextable_

# below: small variations on [rmd_flextable_()]

#' @rdname rmd_
#' @export rmd_.glht
#' @export
rmd_.glht <- function(x, xnm, ...) {
  if (!is.character(xnm)) xnm <- deparse1(xnm)
  return(c(
    rmd_(x$model, xnm = paste0(xnm, '$model'), ...),
    rmd_flextable_(x, xnm = xnm, ...)
  ))
}

#' @rdname rmd_
#' @examples
#' library(DanielBiostatistics10th); list(
#'   '`binTab`' = binTab(array(c(7L, 3L, 8L, 6L), dim = c(2,2)))
#' ) |> render_(file = 'binTab')
#' @export rmd_.binTab
#' @export
rmd_.binTab <- function(x, xnm, ...) {
  return(c(
    rmd_flextable_(x, xnm = xnm),
    '```{r comment = NA}', 
    paste0('print.binTab(', xnm, ', print_flextable = FALSE)'), # how to put in `prevalence` here??
    '```'
  ))
}

#' @rdname rmd_
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render_(file = 'DemographicTable')
#' @export rmd_.DemographicTable
#' @export
rmd_.DemographicTable <- function(x, xnm, font.size = 9, ...) {
  rmd_flextable_(x, xnm = xnm, font.size = font.size, ...)
}


#' @rdname rmd_
#' @export rmd_.summary.matchit
#' @export
rmd_.summary.matchit <- rmd_flextable_ 

#' @rdname rmd_
#' @examples
#' library(MatchIt)
#' m = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde)
#' list(
#'  '`matchit1`' = m,
#'  '`matchit2`' = m |> summary(addlvariables = 're78')
#' ) |> render_(file = 'matchit')
#' @export rmd_.matchit
#' @export
rmd_.matchit <- function(x, xnm, ...) {
  rmd_.summary.matchit(
    x = summary(x), # ?MatchIt:::summary.matchit
    xnm = sprintf(fmt = 'summary(%s)', xnm),
    ...)
}


