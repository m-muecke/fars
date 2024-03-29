% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Load US National Highway Traffic data}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string providing the file path of the csv to load}
}
\value{
This function returns a tibble containing the US National Highway
Traffic Safety Administration's Fatality Analysis Reporting System data.
}
\description{
\code{fars_read} loads-in the US National Highway Traffic Safety
Administration's Fatality Analysis Reporting System data from a provided csv
file. \code{fars_read} will throw an error if the provided file does not
exist.
}
\examples{
\dontrun{
df <- fars_read("accident_2013.csv.bz2")
}
}
