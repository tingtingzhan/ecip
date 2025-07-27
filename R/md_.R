

# library(logistf); library(rmd.tzh); list(
#  '`logistf`' = logistf(case ~ age+oc+vic+vicl+vis+dia, data = sex2)
# ) |> render_(file = 'logistf')
# # I will finalize \pkg{logistf.tzh} when I have time :)
# # this example works fine when logistf::logistf was supported in \pkg{tzh}

#' @importFrom methods new
md_ecip <- function(x, xnm, ...) {
  
  z1 <- Sprintf(x) # S3 generic [Sprintf()]
  
  z2 <- c(
    '```{r}', 
    sprintf(fmt = '(%s) |> ecip() |> as_flextable.ecip()', xnm),
    '```'
  ) |> new(Class = 'md_lines')
  
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
#' @export md_.lm
#' @export
md_.lm <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.lme
#' @export
md_.lme <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.gls
#' @export
md_.gls <- md_ecip


#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.glmgee
#' @export
md_.glmgee <- md_ecip


#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.clm
#' @export
md_.clm <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.clmm
#' @export
md_.clmm <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.coxph
#' @export
md_.coxph <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.merMod
#' @export
md_.merMod <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.rlm
#' @export
md_.rlm <- md_ecip

#' @rdname md_
#' @importFrom rmd.tzh md_
#' @export md_.vlm
#' @export
md_.vlm <- md_ecip

