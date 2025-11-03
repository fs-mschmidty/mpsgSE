---
output: html_document
---

# Mountain Planning Service Group Species Evaluation (`mpsgSE`) R Package

## Overview

Welcome to the Mountain Planning Service Group (MPSG; USDA Forest Service) Species Evaluation R package. This package has been developed by MPSG staff to standardize species data and produce template species evaluation documents that are part of the [Species of Conservation Concern (SCC)](https://www.fs.usda.gov/naturalresources/programs/scc.shtml) process. This package hosts several utilities that standardize species observation data from repositories like the Global Biodiversity Information Facility (GBIF; <https://www.gbif.org/>) and SEINet <https://swbiodiversity.org/seinet/index.php>), query species range data from eBird <https://science.ebird.org/en/status-and-trends>) and the Botanical Information and Ecological Network (<https://bien.nceas.ucsb.edu/bien/>), and produces template species evaluation documents in Microsoft Word format using a `Quarto` scrip.

**Note:** Some functions access publicly available data sources like GBIF, eBird, and BIEN. Other functions access data on internal Forest Service servers and will not work unless you are on the Forest Service network or VPN with the servers mapped to your computer. We have tired to note which functions access Forest Service servers in the man pages. Please let us know if we missed anything by submitting an `Issue`.

**Authors:** [Michael Schmidt](https://github.com/fs-mschmidty) and [Matt Van Scoyoc](https://github.com/fs-scoyoc)

**Maintainer:** [Matt Van Scoyoc](https://github.com/fs-scoyoc)

**Version:** 0.1.0

**License:** Mit + file [LICENSE](https://github.com/fs-mschmidty/mpsgSE/LICENSE)

**Depends:** R (\>= 4.1.0)

**Imports:** arcgislayers, BIEN, devtools, dplyr, ebirdst, glue, httr2, janitor, lubridate, natserv, osmdata, readr, remotes, rgbif, readxl, rnaturalearth, sf, stringr, taxize, tibble, tidyr, tidyselect

**Issues:** This package is under active development and changes often with out warning. Functions may experience breaking changes at any time. If you find a bugs or have an idea for a feature, please submit an Issue at <https://github.com/fs-mschmidty/mpsgSE/issues>.

**Documentation:** Just man pages for now.

## Installation

``` r
remotes::install_github("fs-mschmidty/mpsgSE")
```

## Template Species Evaluation Documents

[Here](fs-mschmidty/mpsgSE/docs/Example_Olive-sided_Flycatcher_Contopus_cooperi.pdf) is an example of a template species evaluation document for Olive-sided Flycatcher on the Bitterroot National Forest. These documnents are templates that are customzed for each land management revision project (i.e., a National Forest or Grassland) and MPSG biologists will add, revise, or remove information using their best professional judgment before this document becomes part of the project record.

## List of Functions

-   `build_bbs_trend_narratives`: Build narratives from USGS Breeding Bird Survey trend data.

-   `build_gbif_spatial_data`: Subset eligible species from spatial GBIF data.

-   `build_imbcr_spatial_data`: Subset eligible species from spatial IMBCR data.

-   `build_imbcr_spp`: Summarize IMBCR data by species.

-   `build_iucn_maps`: Read spatial range data from International Union for Conservation of Nature (IUCN) into R. This function requires access to internal Forest Service servers.

-   `build_map_source`: Build a data frame of map source information for Quarto script.

-   `build_ns_spp_list`: Build a species list with global and state NatureServe conservation rankings from `get_ns_state_list().`

-   `build_seinet_spatial_data`: Subset eligible species from spatial SEINet data and reduce variables.

-   `build_seinet_spp`: Summarize SEINet data by species.

-   `clip_fc`: Clip an `sf` object to polygon.

-   `combine_ns_habs`: Combine two NatureServe habitat data frames from `get_ns_habitat()`.

-   `compile_spp_list`: Combine species lists into one data frame.

-   `count_spp_by_hab`: Count species by NatureServe habitat type.

-   `download_bien_range_maps`: Download BIEN range map data and read them into R.

-   `download_ebird_range_maps`: Download eBird range map data and read them into R.

-   `get_ebird_regional_stats`: Download eBird regional statistics and read them into R.

-   `download_ebird_status_maps`: Download eBird status map data and read them into R.

-   `download_ebird_trends_maps`: Download eBird trend data and read them into R.

-   `gbif_spp`: Summarize GBIF data by species.

-   `get_basemap_data`: Read basemap data into R for Quarto script.

-   `get_bc_regions`: Clip Birds of Conservation Regions to a polygon.

-   `get_bcr_list`: Get USGS Birds of Conservation Concern breeding bird list for National Forest or Grassland.

-   `get_ebird_trends`: Calculate abundance trends from eBird data.

-   `get_gbif`: Query and download species occurrence records from GBIF and read them into R as spatial (`sf`)data.

-   `get_imbcr_data`: Read IMBCR occurrence data into R. This function requires access to internal Forest Service servers.

-   `get_imbcr_trends`: Read IMBCR trend data into R. This function requires access to internal Forest Service servers.

-   `get_iucn_shp_paths`: Retrieve file paths to IUCN shapefiles. This function requires access to internal Forest Service servers.

-   `get_ns_habitat`: Read NatureServe habitat types for a given species into R.

-   `get_ns_state_list`: Import NatureServe species data for a given state into R.

-   `get_rfss_list`: Read a Forest Service Regional Forester's Sensitive Species List into R.

-   `get_seinet_data`: Read SEINet data in Darwin Core Archive format into R.

-   `get_taxonomies`: Query taxon ID and taxonomic information from the GBIF backbone taxonomies database.

-   `get_synonyms`: Query taxonomic synonyms from the GBIF backbone taxonomies database.

-   `list_regions`: List Forest Service regions used in `get_rfss_list()`.

-   `ns_ranks`: **Deprecated**. Generate a species list with NatureServe global and state conservation rankings.

-   `read_edw_lyr`: Read spatial data from Forest Service ArcGIS REST Services into R.

-   `read_fc`: Read feature class from a geodatabase into R.

-   `wkt_string`: Create a well-known text string from an `sf` polygon object for `get_gbif()`.

## List of Data Sets

The data sets listed below are being used in the MPSG species evaluation process and not intended for distribution. Recommended best practice is to obtain these data from their sources.

-   `core_index`: Core Index data from the USGS Breeding Bird Survey (1966-2023).

-   `core_trend`: Core Trend data from the USGS Breeding Bird Survey (1966-2023).

-   `expanded_index`: Expanded Index data from the USGS Breeding Bird Survey (1993-2023).

-   `expanded_trend`: Expanded Trend data from the USGS Breeding Bird Survey (1993-2023).

-   `bcc_list`: 2024 US Fish & Wildlife Birds of Conservation Concern List.

-   `bcc_regions`: North American Bird Conservation Initiative Birds Conservation Regions.

-   `conservation_lists`: **Deprecated**. List of conservation lists including Colorado SWAP, T&E, and USFS Region 2 Sensitive Species List.

-   `fs_units`: National Forests and Grasslands.

-   `id_swap`: Specieis on the 2023 Idaho State Wildlife Action Plan.

-   `mt_swap`: Species on the 2023 Montana State Wildlife Action Plan.

-   `rfss`: 2024 Forets Service Regional Forester's Sensitive Species List for Regions 1-10.

-   `rfss_taxonomy`: Taxonomy for `rfss` using `get_taxonomies()`.

-   `sp_list_ex`: An example data set of common names and scientific names of species.

-   `ut_blm_ss`: 2025 Utah BLM Sensitive Species List

-   `ut_swap`: Species on the 2025 Utah State Wildlife Action Plan.
