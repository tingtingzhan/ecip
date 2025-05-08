

#' @title Wrapper for \link[stats]{.getXlevels}
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @details 
#' Function [xlevels()] is a wrapper of function \link[stats]{.getXlevels}.
#' 
#' @note
#' Function \link[stats]{.getXlevels} only reports \link[base]{factor} levels, 
#' \link[base]{logical} predictors will *not* be included!
#' 
#' @keywords internal
#' @name xlevels
#' @export
xlevels <- function(x) UseMethod(generic = 'xlevels')

# we do need [xlevels] to be S3 generic
# we need dispatch(es), e.g., [xlevels.emmGrid]

#' @rdname xlevels
#' @importFrom stats .getXlevels model.frame
#' @export xlevels.default
#' @export
xlevels.default <- function(x) {
  
  e <- tryCatch(getElement(object = x, name = 'xlevels'), error = identity)
  if (!inherits(e, what = 'error') && length(e)) return(e)
  
  m <- model.frame(x) # use S3, let err
  # how do I activate S4 method dispatch (e.g., VGAM::model.frame) .. I dont know. currently I wrote ?aggRate::xlevels.vlm
  if (!inherits(m, what = 'data.frame')) stop('stats::model.frame dispatch not available for ', class(m)[1L])
  trm <- m |> attr(which = 'terms', exact = TRUE)
    
  .getXlevels(Terms = trm, m = m)
  
}





