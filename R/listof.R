

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


#' @rdname Sprintf
#' @importFrom MASS.tzh .Sprintf.stepAIC
#' @export Sprintf.stepAIC
#' @export
Sprintf.stepAIC <- function(x) {
  c(
    Sprintf(x[[length(x)]]), # requires S3 generic [Sprintf()]
    .Sprintf.stepAIC(x)
  )
}


