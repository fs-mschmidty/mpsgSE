library(tidyverse)
library(targets)
library(janitor)
library(readxl)

devtools::load_all()


data_r <- read_excel("data-raw/data/co/CNHP_Tracking_List_20240421.xlsx") |>
  clean_names() |>
  rename(scientific_name = gname, common_name = scomname) |>
  mutate(scientific_name = str_replace(scientific_name, " pop\\. 1", "")) |>
  filter(majorgroup != "Natural Communities") |>
  filter(!is.na(costatus))

state_status <- data_r |>
  filter(costatus != "-") |>
  mutate(
    status_r = case_when(
      costatus == "SE" ~ "Endangered",
      costatus == "ST" ~ "Threatened",
      costatus == "SC" ~ "Candidate",
      TRUE ~ NA
    ),
    status_area = "Colorado",
    status_type = "State T and E",
    status_authority = "Colorado Natural Heritage Program",
    status_simple = costatus,
    status_all = paste("Colorado", costatus, sep = " ")
  ) |>
  filter(!is.na(status_simple)) |>
  get_taxonomies() |>
  select(taxon_id, scientific_name, common_name, status_area, status_authority, status_all, status_simple, status_type)

write_csv(state_status, "data-raw/output/species_lists/co_te.csv")
