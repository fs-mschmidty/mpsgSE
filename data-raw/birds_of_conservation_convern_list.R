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

# data ----
bcc_file <- file.path("data-raw/data", "BirdsOfConservationConcern_2024.xlsx")

bcc_list <- readxl::read_excel(path = bcc_file, sheet = "Table001 (Page 1-4)", 
                               skip = 1, col_names = TRUE) |> 
  janitor::clean_names() |> 
  mpsgSE::get_taxonomies('scientific_name') |> 
  mpsgSE::correct_taxon_ids()


# save ----
readr::write_csv(bcc_list,
                 file.path("data-raw/output/species_lists/bcc_list.csv"))
usethis::use_data(bcc_list, overwrite = TRUE)



