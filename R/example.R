#' Get path to fars example
#'
#' readr comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' fars_example()
#' fars_example("accident_2013.csv.bz2")
fars_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "fars"))
  } else {
    system.file("extdata", file, package = "fars", mustWork = TRUE)
  }
}
