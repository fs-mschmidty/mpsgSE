% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{compile_spp_list}
\alias{compile_spp_list}
\title{Combine Species Lists}
\usage{
compile_spp_list(fs_unit_list, buffer_list)
}
\arguments{
\item{fs_unit_list}{Species list for FS Unit.}

\item{gbif_buff}{Species list for 1-km Buffer.}
}
\value{
A tibble.
}
\description{
Create a comprehensive species list from occurrence records on Forest Service
(FS) land and the 1-km buffer of FS land.
}
\examples{
## Not run:

library("mpsgSE")

# Read spatial data into R
t_path <- file.path("T:/path/to/project/directory")
gdb_path <- file.path(t_path, "GIS_Data.gdb")
sf_fs <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
sf_buff <- read_fc(lyr = "PlanArea_1kmBuffer", dsn = gdb_path, crs = "NAD83")

# Pull data from existing GBIF query
gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
                     t_path = file.path(t_path, "data"))

# Clip to extents
# Summarize species
gbif_list <- compile_spp_list(unit_gbif, buff_gbif)

## End(Not run)                     
}
\seealso{
\code{\link[=gbif_spp]{gbif_spp()}}, \code{\link[=seinet_spp]{seinet_spp()}}, \code{\link[=imbcr_spp]{imbcr_spp()}}
}
