#' ---
#' title: "Taxonomy and Taxon ID's for the Utah SWAP List"
#' author:
#'   - name: "Matthew Van Scoyoc" 
#'     affiliation: |
#'       | Mountain Planning Service Group, Regions 1-4
#'       | Information Management Group
#'       | Forest Service, USDA
#' date: 17 July, 2025
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
excel_path <- file.path("data-raw/data", "UTAH SWAP.xlsx")

ut_swap <- readxl::read_excel(excel_path, sheet = "UTAH SWAP", skip = 1,
                              col_names = TRUE) |> 
  janitor::clean_names() |> 
  mpsgSE::get_taxonomies('scientific_name') |> 
  dplyr::select(taxon_id, gbif_taxonID, scientific_name, common_name, 
                s_rank_1:federal_status, kingdom:species, subspecies, variety)

# save ----
readr::write_csv(ut_swap,
                 file.path("data-raw/output/species_lists/ut_swap.csv"))
usethis::use_data(ut_swap, overwrite = TRUE)
