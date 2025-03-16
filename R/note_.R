
#' @title note_
#' 
#' @param x an R object
#' 
#' @name note
#' @export
note_ <- function(x) UseMethod(generic = 'note_')


#' @rdname note
#' @importFrom utils methods
#' @export
note_.default <- function(x) {
  
  has_cut <- if (isS4(x)) {
    # tzh is not familiar with the S4 version here..
    FALSE
  } else {
    methods(class = class(x)[1L]) |>
      attr(which = 'info', exact = TRUE) |>
      getElement(name = 'generic') |> 
      `%in%`(x = 'cut')
  }
  
  if (!has_cut) return(character())
  
  # e.g.,
  # ?ltm.tzh::cut.cronbachAlpha
  x |> cut() |> as.character.factor()
  
}


