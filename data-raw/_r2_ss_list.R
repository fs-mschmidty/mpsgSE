#' This script builds the Region 2 Sensitive Species List and outputs it in a uniform output format that can be combined with other conservation list.
library(tidyverse)
library(readxl)
library(janitor)
devtools::load_all()

r2_ss_list <- read_excel("data-raw/data/fs/2023_R2_RegionalForestersSensitiveSppList.xlsx") |>
  clean_names() |>
  select(common_name, scientific_name) |>
  mutate(
    status_area = "USFS Region 2",
    status_authority = "US Forest Service",
    status_all = "USFS R2 Sensitive Species",
    status_simple = "Sensitive Species",
    status_type = "Sensitive Species"
  ) |>
  get_taxonomies()

r2_ss_list |>
  select(taxon_id, scientific_name, common_name, status_area, status_authority, status_all, status_simple, status_type) |>
  write_csv("data-raw/output/species_lists/r2_ss_list.csv")
