

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
  
}



