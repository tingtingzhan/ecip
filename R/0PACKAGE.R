

#' @import methods
#' 
#' @import aggRate
#' @import DanielBiostatistics10th
#' @import flextable.tzh
#' @import glmtoolbox.tzh
#' @import lcmm.tzh
#' @import lme4.tzh
#' @import ltm.tzh
#' @import multcomp.tzh
#' @import nlme.tzh
#' @import pscl.tzh
#' @import survival.tzh
#' @import vcd.tzh
'_PACKAGE'

#

# @useDynLib tzh, .registration = TRUE # needed if using Rcpp





# @details
# 
# Java support is not required.  In other words, packages depending on \pkg{rJava} will 
# not be used, because they may not keep up to date with the latest version of Java.
# 
# **On new installation of fresh R**
# 
# 
# \itemize{
# 
# \item {\url{https://r-pkgs.org/git.html}}
# 
# \item {\url{https://r-pkgs.org/man.html#links}}
# 
# \item {For Unicode support, one may need to run (once and for all)\cr
# `extrafont::font_import()`\cr\cr
# See \url{https://cran.r-project.org/web/packages/extrafont/README.html} for detail.}
#
#
# }
#
# **Creating Vignette Directory**
# 
# **Warning!** This is for demonstration only!  Running these code will delete existing vignette!!
# \code{# usethis::use_vignette('qfit', title = 'Quantile Fitting Algorithm')}
#
# **Pending projects**
# 
# \itemize{
# 
# \item {Add Cohen's d (`effsize::cohen.d`) for the numeric rows of 'DemographicTable'.}
# 
# \item {Path analysis via \pkg{sem}}
# 
# \item {\url{https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/}}
# 
# \item {Interim analysis boundary for logrank test (on survival study)}
# 
# \item {Interim analysis: alpha spending function (DeMET & Lan 1994) applicable to
# comparison of means, proportions, survival & longitudinal.
# Definition of 'information fraction'.}
# 
# }
# 
# @references
# \url{https://r-pkgs.org/namespace.html}
# \url{https://stackoverflow.com/questions/18512528/how-to-export-s3-method-so-it-is-available-in-namespace}
# 