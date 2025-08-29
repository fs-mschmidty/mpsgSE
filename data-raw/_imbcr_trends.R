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


# data ----
imbcr_dir <- file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species", 
                       "IMBCR_USFSdata/trends")

## 2024-01-12 Data
# imbcr_xlsx <- file.path(imbcr_dir, "CCNG_population_estimates_20240112.xlsx")
# trend_202401 <- readxl::read_excel(imbcr_xlsx, sheet = "trend") |> 
#   janitor::clean_names()
# dens_occ_202401 <- readxl::read_excel(imbcr_xlsx, sheet = "Density_Occ") |> 
#   janitor::clean_names()
# 
## 2024-03-28 Data
# imbcr_xlsx <- file.path(imbcr_dir, "CCNG_population_estimates_20240328.xlsx")
# trend_202403 <- readxl::read_excel(imbcr_xlsx, sheet = "trend") |> 
#   janitor::clean_names()
# dens_occ_202403 <- readxl::read_excel(imbcr_xlsx, sheet = "Density_Occ") |> 
#   janitor::clean_names()

# 2025-04-23 Data
imbcr_xlsx <- file.path(imbcr_dir, "CO_MT trends thru 2024.xlsx")
trend_202504 <- readxl::read_excel(imbcr_xlsx, sheet = "Sheet1") |> 
  janitor::clean_names()

# 2025-08-20
imbcr_xlsx <- file.path(imbcr_dir, "IMBCR BCR18 trends.xlsx")
trend_202508 <- readxl::read_excel(imbcr_xlsx, sheet = "trends") |> 
  janitor::clean_names()

# 2025-08-21
imbcr_xlsx <- file.path(imbcr_dir, "additional_trends.xlsx")
additional <- readxl::read_excel(imbcr_xlsx, sheet = "Sheet1") |> 
  janitor::clean_names()

imbcr_trends <- dplyr::bind_rows(trend_202508, additional) |> 
  dplyr::mutate(
    percent_change_yr_density = as.numeric(percent_change_yr_density),
    percent_change_yr_occupancy = as.numeric(percent_change_yr_occupancy)
    ) |> 
  dplyr::bind_rows(trend_202504)


# save ----
saveRDS(imbcr_trends, file = file.path(imbcr_dir, "imbcr_trends.RDS"))


# imbcr_trends <- readRDS(file = file.path(imbcr_dir, "imbcr_trends.RDS"))

