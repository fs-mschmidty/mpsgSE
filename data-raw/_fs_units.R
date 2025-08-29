#' ---
#' title: "List of Forest Service Units"
#' author:
#'   - name: "Matthew Van Scoyoc" 
#'     affiliation: |
#'       | Mountain Planning Service Group, Regions 1-4
#'       | Information Management Group
#'       | Forest Service, USDA
#' date: 24 July, 2025
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

# OOPS!!!

# save ----
readr::write_csv(fs_units,
                 file.path("data-raw/output/species_lists/fs_units.csv"))
usethis::use_data(fs_units, overwrite = TRUE)
