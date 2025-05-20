#' ---
#' title: "Taxonomy and Taxon ID's for the Montana SWAP List"
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
mt_swap_file <- file.path("data-raw/data", "Montana_SGCN_SWAPSpecies.xlsx")

mt_swap <- readxl::read_excel(mt_swap_file, sheet = "Combined List", 
                              col_names = TRUE, skip = 4) |>  
  janitor::clean_names() |> 
  dplyr::select(group, scientific_name, common_name) |> 
  dplyr::filter(!is.na(scientific_name)) |> 
  mpsgSE::get_taxonomies('scientific_name')


# save ----
readr::write_csv(mt_swap,
                 file.path("data-raw/output/species_lists/mt_swap.csv"))
usethis::use_data(mt_swap, overwrite = TRUE)

