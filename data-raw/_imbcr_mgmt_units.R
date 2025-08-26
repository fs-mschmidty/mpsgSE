#' ---
#' title: "Taxonomy and Taxon ID's for the USFWS Birds of Conservation Concern List"
#' author:
#'   - name: "Matthew Van Scoyoc" 
#'     affiliation: |
#'       | Mountain Planning Service Group, Regions 1-4
#'       | Information Management Group
#'       | Forest Service, USDA
#' date: 22 April, 2025
#' 
#' This script queries taxonomy and taxon IDs using the `get_taxonomies()` 
#' function.
#'-----------------------------------------------------------------------------


# setup ----
pkgs <- c("dplyr",   # data management
          "janitor", # tidy up data frames
          "mpsgSE",  # taxonomy function
          "readr",   # read/write data
          "readxl",  # read Excel files
          "tidyr",   # data management
          "usethis") # package development

# Install packages if they aren't in your library
inst_pkgs <- pkgs %in% rownames(installed.packages())
if (any(inst_pkgs == FALSE)) {
  install.packages(pkgs[!inst_pkgs], 
                   lib =  .libPaths()[1], 
                   repos = "https://cloud.r-project.org",
                   type = 'source', 
                   dependencies = TRUE, 
                   quiet = TRUE)
}

# Load packages
invisible(lapply(pkgs, library, character.only = TRUE))

rda_path = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species",
                         "2023_IMBCR_USFSdata/imbcr_mgmt_units.RDS")
imbcr_mgmt_units <- readRDS(rda_path) |> tibble::tibble()

# save ----
readr::write_csv(imbcr_mgmt_units,
                 file.path("data-raw/output/species_lists/imbcr_mgmt_units.csv"))
usethis::use_data(imbcr_mgmt_units, overwrite = TRUE)



