% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{fars_example}
\alias{fars_example}
\title{Get path to fars example}
\usage{
fars_example(file = NULL)
}
\arguments{
\item{file}{Name of file. If \code{NULL}, the example files will be listed.}
}
\description{
readr comes bundled with a number of sample files in its \code{inst/extdata}
directory. This function make them easy to access
}
\examples{
fars_example()
fars_example("accident_2013.csv.bz2")
}
