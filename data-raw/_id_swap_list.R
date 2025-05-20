#' ---
#' title: "Taxonomy and Taxon ID's for the Idah SWAP List"
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
id_swap_file <- file.path("data-raw/data", "IDSWAP2023Spp_v20250506.xlsx")

id_swap <- readxl::read_excel(id_swap_file, sheet = "SWAPSpecies", 
                              col_names = TRUE) |> 
  dplyr::select(taxonID, scientificName, sPrimaryCommonName, synonymName, 
                SWAP2015, SWAP2023, SWAP2023designation, lastUpdated, 
                taxonomyNotes, comments) |> 
  dplyr::rename("ID_taxonID" = taxonID) |> 
  mpsgSE::get_taxonomies('scientificName')


# save ----
readr::write_csv(id_swap,
                 file.path("data-raw/output/species_lists/id_swap.csv"))
usethis::use_data(id_swap, overwrite = TRUE)

