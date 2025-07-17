library(tidyverse)
library(janitor)

core_trend <- readr::read_csv(
  "data-raw/data/bbs/BBS_Core_Trends_1966-2023.csv", 
  show_col_types = FALSE
  ) |>
  janitor::clean_names()
usethis::use_data(core_trend, overwrite = TRUE)

expanded_trend <- readr::read_csv(
  "data-raw/data/bbs/BBS_Expanded_Trends_1993-2023.csv", 
  show_col_types = FALSE
  ) |>
  janitor::clean_names()
usethis::use_data(expanded_trend, overwrite = TRUE)

core_index <- readr::read_csv(
  "data-raw/data/bbs/BBS_Core_Indices_1966-2023.csv", 
  show_col_types = FALSE
  ) |>
  janitor::clean_names() |>
  dplyr::rename(ci_2.5_percenti = x2_5_percent_ci, 
                ci_97.5_percent = x97_5_percent_ci)
usethis::use_data(core_index, overwrite = TRUE)

expanded_index <- readr::read_csv(
  "data-raw/data/bbs/BBS_Expanded_Indices_1993-2023.csv", 
  show_col_types = FALSE
  ) |>
  janitor::clean_names() |>
  dplyr::rename(ci_2.5_percenti = x2_5_percent_ci, 
                ci_97.5_percent = x97_5_percent_ci)
usethis::use_data(expanded_index, overwrite = TRUE)
