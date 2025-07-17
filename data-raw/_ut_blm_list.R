#' ---
#' title: "Taxonomy and Taxon ID's for the Utah BLM Sensitive Species List"
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
excel_path <- file.path("data-raw/data", "BLM_UTAH_SS.xlsx")

plants <- readxl::read_excel(excel_path, sheet = "Plants", skip = 1,
                                col_names = TRUE) |> 
  janitor::clean_names() |> 
  dplyr::select(scientific_name, common_name, blm_status)

wildlife <- readxl::read_excel(excel_path, sheet = "Wildlife", skip = 1,
                             col_names = TRUE) |> 
  janitor::clean_names() |> 
  dplyr::select(scientific_name, common_name, blm_status)

ut_blm_ss <- rbind(plants, wildlife) |> 
  mpsgSE::get_taxonomies('scientific_name') |> 
  dplyr::select(taxon_id, gbif_taxonID, scientific_name, common_name, 
                blm_status, kingdom:species, subspecies, variety)



# save ----
readr::write_csv(ut_blm_ss,
                 file.path("data-raw/output/species_lists/ut_blm_ss.csv"))
usethis::use_data(ut_blm_ss, overwrite = TRUE)
