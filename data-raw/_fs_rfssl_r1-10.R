#' ---
#' title: "Taxonomy and Taxon ID's for Regional Foresters Sensitive Species Lists"
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
          "mpsgSE",  # taxonomy function 
          "readxl",  # read Excel files
          "tidyr")   # data management

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
#-- raw data
rfss_file <- file.path("data-raw", "data", "2024_RFSS_Lists_R1-10.xlsx")
rfss_raw <- readxl::read_excel(rfss_file, sheet = "RFSSL Master List") |> 
  dplyr::select(-fs_name) |> 
  dplyr::distinct() |> 
  mpsgSE::get_taxonomies() |> 
  mpsgSE::correct_taxon_ids()

#-- regional lists
rfss <- rfss_raw |> 
  dplyr::select(taxon_id, scientific_name:R10) |> 
  tidyr::pivot_longer(R1:R10, names_to = "region", values_to = "rfss") |> 
  dplyr::mutate(rfssl = ifelse(rfss == "X", TRUE, FALSE), 
                region = factor(region, 
                                levels = c("R1", "R2", "R3", "R4", "R5", "R6", 
                                           "R8", "R9", "R10"))) |> 
  dplyr::filter(rfssl == TRUE) |>
  dplyr::mutate(
    region_num = stringr::str_remove(region, "R"),
    status_area = paste("USFS Region", region_num, sep = " "),
    status_authority = "US Forest Service",
    status_all = paste("USFS", region, "Sensitive Species", sep = " "),
    status_simple = "Sensitive Species",
    status_type = "Sensitive Species"
    ) |>
  dplyr::select(-rfss) |> 
  dplyr::arrange(group_level2, scientific_name, region)

#-- taxonomies
taxa_select = c("taxon_id", "scientific_name", "kingdom", "phylum", "class",
                "order", "family", "genus", "species", "subspecies", "variety", 
                "form")
rfss_taxonomy <- dplyr::select(rfss_raw, dplyr::any_of(taxa_select))

# save ----
readr::write_csv(rfss,
                 file.path("data-raw/output/species_lists/fs_rfss_lists.csv"))
readr::write_csv(rfss_taxonomy,
                 file.path("data-raw/output/species_lists/fs_rfss_taxonomy.csv"))

# rfss <- readr::read_csv(
#   file.path("data-raw/output/species_lists/fs_rfss_lists.csv")
#   )
# rfss_taxonomy <- readr::read_csv(
#   file.path("data-raw/output/species_lists/fs_rfss_taxonomy.csv")
# )
usethis::use_data(rfss, rfss_taxonomy, overwrite = TRUE)

