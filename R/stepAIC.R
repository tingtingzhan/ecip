


#' @title Convert \link[MASS]{stepAIC} Return to \link[base]{matrix}
#' 
#' @description
#' S3 method dispatch for S3 generic \link[base]{as.matrix}.
#' 
#' @param x returned object from function \link[MASS]{stepAIC}, or \link[MASS.tzh]{stepAIC_complete}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom flextable as_flextable color add_footer_lines
#' @export as.matrix.stepAIC
#' @export
as.matrix.stepAIC <- function(x, ...) {
  
  # '\u274c' # unicode 'Cross Mark'
  # '\U1f6ab' # unicode 'No Entry Sign'
  
  nx <- length(x)
  
  z <- x |> lapply(FUN = ecip)
  
  # all intermediate models, only print p-values
  id0 <- seq_len(nx-1L)
  m0 <- z[id0] |> lapply(FUN = as.matrix.ecip, type = 'p_only')
  
  # final model
  m <- z[[nx]] |> as.matrix.ecip(type = 'ncol1')
  
  r0 <- m0 |>
    lapply(FUN = rownames) |>
    unlist(use.names = FALSE) |>
    unique.default() # do *not* ?base::sort !!!
  
  out0 <- array(data = '\u274c', dim = c(length(r0), length(m0)), dimnames = list(r0, NULL))
  for (i in id0) {
    out0[match(x = rownames(m0[[i]]), table = r0), i] <- m0[[i]]
  }
  
  r1 <- setdiff(rownames(m), r0)
  
  # `out0`, augmented
  out0a <- if (length(r1)) { # final model has more predictors (e.g., stepAIC_complete(., upper = ~ a1 + a2))
    rbind(out0, array('\U1f6ab', dim = c(length(r1), ncol(out0)), dimnames = list(r1, NULL)))
  } else out0 # final model does *not* have more predictors
  
  out1 <- cbind(out0a, if (length(attr(x, which = 'upper', exact = TRUE))) '\U1f6ab' else '\u274c')
  out1[match(rownames(m), table = rownames(out1)), nx] <- m
  
  colnames(out1) <- c(m0, list(m)) |> vapply(FUN = colnames, FUN.VALUE = '', USE.NAMES = FALSE)
  colnames(out1) <- paste(
    paste0('(', names(x), ')'),
    colnames(out1), 
    sep = '\n')
  
  attr(out1, which = 'row.title') <- x[[1L]] |> endpoint() |> deparse1()
  attr(out1, which = 'hline_i') <- nrow(out0)
  attr(out1, which = 'vline_j') <- id0
  return(out1)
  
}





#' @title Turn \link[MASS.tzh]{stepAIC_complete} Return to \link[flextable]{flextable}
#' 
#' @param x returned value of function \link[MASS.tzh]{stepAIC_complete}
#' 
#' @param row.title,hline_i,vline_j ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable color
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.stepAIC
#' @export
as_flextable.stepAIC <- function(
    x, 
    row.title = z |> attr(which = 'row.title', exact = TRUE),
    hline_i = z |> attr(which = 'hline_i', exact = TRUE),
    vline_j = z |> attr(which = 'vline_j', exact = TRUE), # needed for `color_j`
    ...
) {
  z <- as.matrix.stepAIC(x, ...)
  z |>
    as_flextable.matrix(
      row.title = row.title,
      hline_i = hline_i
    ) |>
    color(j = vline_j + 1L, color = 'grey60', part = 'all') |>
    add_footer_lines(values = c(
      '\u274c: predictor(s) removed by stepwise algorithm.',
      '\U1f6ab: predictor(s) not considered in the model.'
    ))
}







#' @rdname Sprintf
#' @importFrom MASS.tzh .Sprintf.stepAIC
#' @export Sprintf.stepAIC
#' @export
Sprintf.stepAIC <- function(x) {
  return(list(
    Sprintf(x[[length(x)]]),
    .Sprintf.stepAIC(x) # !!! probably bib from here too!!
  ))
}


#' @rdname md_
#' @export md_.stepAIC
#' @export
md_.stepAIC <- md_flextable_




