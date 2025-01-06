library(tidyverse)
library(janitor)

core_trend <- read_csv("data-raw/data/bbs/BBS_1966-2022_core_trend2.csv") |>
  clean_names()
usethis::use_data(core_trend, overwrite = TRUE)

expanded_trend <- read_csv("data-raw/data/bbs/BBS_1993-2022_expanded_trend2.csv") |>
  clean_names()
usethis::use_data(expanded_trend, overwrite = TRUE)

aous_rocky_mountains <- core_trend |>
  filter(region == "S16") |>
  select(aou, species_name) |>
  count(aou, species_name) |>
  select(-n)

core_index <- read_csv("data-raw/data/bbs/Index_1966-2022_core2.csv") |>
  clean_names() |>
  left_join(aous_rocky_mountains, by = "aou") |>
  rename(ci_2.5_percenti = x2_5_percent_ci, ci_97.5_percent = x97_5_percent_ci) |>
  filter(!is.na(species_name))

usethis::use_data(core_index, overwrite = TRUE)

expanded_index <- read_csv("data-raw/data/bbs/Index_1993-2022_expanded2.csv") |>
  clean_names() |>
  left_join(aous_rocky_mountains, by = "aou") |>
  rename(ci_2.5_percenti = x2_5_percent_ci, ci_97.5_percent = x97_5_percent_ci) |>
  filter(!is.na(species_name))

usethis::use_data(expanded_index, overwrite = TRUE)
