

# library(logistf); library(rmd.tzh); list(
#  '`logistf`' = logistf(case ~ age+oc+vic+vicl+vis+dia, data = sex2)
# ) |> render_(file = 'logistf')
# # I will finalize \pkg{logistf.tzh} when I have time :)
# # this example works fine when logistf::logistf was supported in \pkg{tzh}





#' @title Writing R Objects to \pkg{rmarkdown} Document
#' 
#' @param x .. 
#' 
#' @param xnm ..
#' 
#' @param font.size see function \link[flextable]{set_flextable_defaults}
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
#' library(nlme); list(
#'  '`lme`' = lme(distance ~ age, data = Orthodont, keep.data = TRUE),
#'  '`gls`' = gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
#'      data = Ovary, correlation = corAR1(form = ~ 1 | Mare))
#' ) |> render_(file = 'lme_gls')
#' 
#' library(glmtoolbox); list(
#'  '`glmgee`' = glmgee(breaks ~ tension, id = wool, data = warpbreaks, corstr = 'exchangeable')
#' ) |> render_(file = 'glmgee')
#' 
#' library(multcomp); list(
#'  '`glht` via `aov`' = aov(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett')),
#'  '`glht` via `lm`, single `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey')),
#'  '`glht` via `lm`, multiple `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett'))
#' ) |> render_(file = 'glht')
#' 
#' library(ordinal); list(
#'  '`clm`' = clm(rating ~ temp + contact, data = wine), 
#'  '`clmm`' = clmm(rating ~ temp + contact + (1|judge), data = wine)
#' ) |> render_(file = 'clm_clmm')
#' 
#' library(survival); list(
#'  '`coxph`' = rotterdam |>
#'  within.data.frame(expr = {
#'   os = Surv(dtime, death)
#'  }) |>
#'  coxph(formula = os ~ size)
#' ) |> render_(file = 'coxph')
#' 
#' library(lme4); list(
#'  '`glmerMod`' = glmer(cbind(incidence, size-incidence) ~ period + (1|herd), 
#'      data = cbpp, family = binomial)
#' ) |> render_(file = 'glmerMod')
#'   
#' library(MASS); list(
#'  '`rlm`' = rlm(stack.loss ~ ., data = stackloss)
#' ) |> render_(file = 'rlm')
#' 
#' library(VGAM)
#' pneumo = transform(pneumo, let = log(exposure.time))
#' list(
#'  '`vglm`' = vglm(cbind(normal, mild, severe) ~ let, propodds, data = pneumo)
#' ) |> render_(file = 'vglm')
#' 
#' @name md_
#' @importFrom rmd.tzh md_
#' @export md_.default
#' @export
md_.default <- function(x, xnm, ...) {
  # `md_ecip_flextable`
  
  return(list(
    
    Sprintf(x), # S3 generic [Sprintf()]
    
    '```{r}', 
    sprintf(fmt = '(%s) |> ecip() |> as_flextable.ecip()', xnm),
    '```',
    '<any-text>',
    
    '\n\n'
    
  ))
  
}



