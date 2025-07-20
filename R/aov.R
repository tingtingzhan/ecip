
#' @rdname Sprintf
#' @export Sprintf.aov
#' @export
Sprintf.aov <- function(x) {
  'Analysis of variance is performed using <u>**`R`**</u>.'
}


#' @rdname Sprintf
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export Sprintf.TukeyHSD
#' @export
Sprintf.TukeyHSD <- function(x) {
  
  'Tukey Honest Significant Differences [HSD, @Tukey49] is provided using <u>**`R`**</u>.' |>
    new(Class = 'md_lines', bibentry = bibentry(
      bibtype = 'Article', key = 'Tukey49',
      author = 'John W. Tukey',
      journal = 'Biometrics',
      number = '2',
      pages = '99--114',
      title = 'Comparing Individual Means in the Analysis of Variance',
      volume = '5',
      year = '1949',
      doi = '10.2307/3001913'
    ))
  
}