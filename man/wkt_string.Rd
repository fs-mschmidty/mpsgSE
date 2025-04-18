% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gbif.R
\name{wkt_string}
\alias{wkt_string}
\title{Create a well-known text string (WTK) string}
\usage{
wkt_string(my_polygon)
}
\arguments{
\item{my_polygon}{An \code{sf} polygon object.}
}
\value{
A vector.
}
\description{
Creates a well-known text string from a polygon (\code{sf} object). This function
transforms the input polygon to WGS84 prior to calculating the wkt string.
}
\examples{
## Not run:

library("mpsgSE")

# Read spatial data into R
t_path <- file.path("T:/path/to/project/directory")
gdb_path <- file.path(t_path, "GIS_Data.gdb")
sf_aoa <- read_fc(lyr = "AdminBdy_1kmBuffer", dsn = gdb_path, crs = "NAD83")

# Create WKT string
wkt_string(sf_aoa)

## End (Not run)
}
