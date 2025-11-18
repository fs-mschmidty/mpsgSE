#' 2024 US Fish & Wildlife Birds of Conservation Concern List
#'
#' @format A data frame of 333 observations and 17 variables.
#' \describe{
#'   \item{common_name}{Common name of bird species.}
#'   \item{scientific_name}{Scientific name of bird species.}
#'   \item{mbta}{Migratory Bird Treaty Act designation.}
#'   \item{federally_endangered}{Location description of where the species is Endangered.}
#'   \item{federally_threatened}{Location description of where the species is Threatened.}
#'   \item{bcc_rangewide_in_continental_us_and_or_pr_and_vi_or_hi_and_pacific_islands}{description}
#'   \item{bc_rs_for_bcc_listing_non_breeding}{List of Bird Conservation Region codes for non-breeding birds.}
#'   \item{bc_rs_for_bcc_listing_breeding}{List of Bird Conservation Region codes for breeding birds.}
#'   \item{gbif_taxonID}{GBIF taxon ID}
#'   \item{kingdom}{Taxonomic Kingdom}
#'   \item{phylum}{Taxonomic Phylum}
#'   \item{class}{Taxonomic Class}
#'   \item{order}{Taxonomic Order}
#'   \item{family}{Taxonomic Family}
#'   \item{genus}{Taxonomic Genus}
#'   \item{species}{Taxonomic Species}
#'   \item{taxon_id}{MPSG taxon ID.}
#' }
#' @source https://www.fws.gov/media/usfws-bird-species-concern
"bcc_list"


#' North American Bird Conservation Initiative Birds Conservation Regions
#'
#' @format An `sf` object (polygon) of 4414 observations and 9 variables.
#' \describe{
#'   \item{bcr_label}{Birds Conservation Region codes.}
#'   \item{bcr_label_name}{Birds Conservation Regions names.}
#'   \item{name_en}{Birds Conservation Regions names in English.}
#'   \item{name_fr}{Birds Conservation Regions names in French.}
#'   \item{name_sp}{Birds Conservation Regions names in Spanish}
#'   \item{globalid}{Polygon ID.}
#'   \item{SHAPE_Length}{Polygon length.}
#'   \item{SHAPE_Area}{Polygon area.}
#'   \item{SHAPE}{Polygon geometry.}
#' }
#' @source https://www.birdscanada.org/bird-science/nabci-bird-conservation-regions
"bcc_regions"


#' List of Forest Service Units
#'
#' @format A data frame of 114 observations and 6 variables.
#' \describe{
#'   \item{adminforestid}{Administrative forest ID.}
#'   \item{region}{Forest Service Region code.}
#'   \item{forestnumber}{Forest number.}
#'   \item{forestorgcode}{Forest org code.}
#'   \item{forestname}{Forest name.}
#'   \item{gis_acres}{Fores acres calsulated by ESRI ArcGIS.}
#' }
#' @source Forest Service EDW Rest Services, https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer
"fs_units"


#' List of National Forests and Grasslands IMBCR survyes on.
#'
#' @format ## `imbcr_mgmt_units`
"imbcr_mgmt_units"


#' An example dataset of common names and scientific names of species.
#'
#' @format A data frame of 427 observations and 2 variables.
#' \describe{
#'   \item{common_name}{Common name of species.}
#'   \item{scientific_name}{Scientific name of species.}
#' }
"sp_list_ex"
