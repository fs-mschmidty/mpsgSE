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
