#' This function combines all of the lists generated with _names in the raw-data folder.
library(tidyverse)
library(devtools)

conservation_lists <- read_csv(list.files("data-raw/output/species_lists", full.names = T))

use_data(conservation_lists, overwrite = T)
