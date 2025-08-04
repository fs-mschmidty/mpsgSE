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
pkgs <- c("sf",      # spatial data tools
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

# data ----
gdb <- file.path("T:/FS/NFS/PSO/MPSG/Data/ExternalData/USGS", 
                 "Breeding_Bird_Survey/BCR_Master.gdb")
bcc_regions <- sf::read_sf(dsn = gdb, layer = "BCR_Terrestrial_Master") |>
  sf::st_make_valid() |> 
  suppressWarnings()


# save ----
usethis::use_data(bcc_regions, overwrite = TRUE)



