

#' @title Convert `'listof'` to \link[flextable]{flextable}
#' 
#' @param x `'listof'`
#' 
#' @param ... ..
#' 
#' @note
#' This is quite exploratory
#' 
#' @importFrom flextable as_flextable
#' @importFrom flextable.tzh as_flextable.matrix
#' @export as_flextable.listof
#' @export
as_flextable.listof <- function(x, ...) {
  
  switch(class(x[[1L]]), ecip = {
    
    # i.e., a list of ?ltm::cronbach.alpha
    
    y <- x |> 
      lapply(FUN = \(i) {
        i |>
          intercept_rm.ecip() |>
          as.matrix.ecip()
      })
    
    y |> 
      do.call(what = rbind) |>
      as_flextable.matrix(
        row.title = x[[1L]]@endpoint,
        hline_i = y |> vapply(FUN = nrow, FUN.VALUE = NA_integer_) |> cumsum()
      )
    
  })
  
}

#' @rdname Sprintf
#' @export Sprintf.listof
#' @export
Sprintf.listof <- function(x) character()


