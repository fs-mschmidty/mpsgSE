% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bcc_list.R
\name{get_bcc_list}
\alias{get_bcc_list}
\title{Get USGS Birds of Conservation Concern (BCC) breeding bird list for a given
Bird Conservation Region.}
\usage{
get_bcc_list(bcc_region)
}
\arguments{
\item{bcc_region}{The numeric BCC region code wrapped in quotes.}
}
\value{
A data frame.
}
\description{
This function reads the \link{bcc_list} data set and filters the list by a
given Bird Conservation Region.
}
\examples{
install.packages(mpsgSE)
bcc_9 <- get_bcc_list(9)
}
