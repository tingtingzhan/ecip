

#' @title Convert an `eciplist` to \link[flextable]{flextable}
#' 
#' @param x an `eciplist`, i.e., a \link[base]{list} of \linkS4class{ecip} objects
#' 
#' @param ... potential parameters of the function [as_flextable.ecip()] 
#' 
#' @keywords internal
#' @importFrom flextable as_flextable wrap_flextable
#' @importFrom patchwork plot_layout
#' @export as_flextable.eciplist
#' @export
as_flextable.eciplist <- function(x, ...) {
  
  x |>
    lapply(FUN = as_flextable.ecip, ...) |>
    lapply(FUN = wrap_flextable) |>
    Reduce(f = `+`) +
    plot_layout(ncol = 1L)
  
  # Patching happens **before** markdown-bib-referencing via \pkg{ftExtra}
  # have not found a good solution yet
  
}

# @importFrom fastmd md_ md_.list
# @export
#md_.eciplist <- function(x, xnm, ...) {
#  xnm |>
#    sprintf(fmt = 'as_flextable(%s)') |>
#    md_.list(xnm = _)
#  haha tzh does not remember how to make this work..
#}
