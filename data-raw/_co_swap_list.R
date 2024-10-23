library(tidyverse)
library(targets)
library(janitor)
library(readxl)

devtools::load_all()

data_raw <- read_excel("data-raw/data/co/CO_SWAP_Chapter2.xlsx", sheet = 1)
data_cl <- data_raw |>
  janitor::clean_names() |>
  mutate(common_name = ifelse(is.na(common_name), x2, common_name)) |>
  filter(!is.na(common_name)) |>
  mutate(priority_tier = ifelse(is.na(priority_tier), x4, priority_tier)) |>
  select(scientific_name = species, common_name, priority_tier) |>
  filter(priority_tier != "Priority Tier") |>
  mutate(
    status_area = "Colorado",
    status_authority = "Colorado Parks and Wildlife",
    status_type = "SWAP",
    status_simple = priority_tier,
    status_all = paste("Colorado SWAP", priority_tier, sep = " ")
  ) |>
  select(-priority_tier) |>
  get_taxonomies() |>
  select(taxon_id, scientific_name, common_name, status_area, status_authority, status_all, status_simple, status_type)


write_csv(data_cl, "data-raw/output/species_lists/co_swap.csv")
