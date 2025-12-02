#' Subset eligible species from SEINet data and reduce variables.
#'
#' @param sei_data Spatial SEINet data from [get_seinet_data()].
#' @param spp_list Species list that includes taxon ID from [get_taxonomies()].
#'                     This is the list that is used to subset the spatial data.
#'
#' @return An `sf` class object.
#'
#' @details
#' Additional details...
#'
#' @seealso [get_seinet_data()], [build_seinet_spp()], [get_taxonomies()]
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#'
#' # Path to data folder
#' data_folder <- file.path("T:/path/to/project/directory", "data/SEINet")
#'
#' # Read SEINet data into R
#' sei_dat <- get_seinet_data(data_folder, crs = "NAD83")
#' # Create species list
#' spp_list <- sei_spp(sei_dat)
#' # Subset species list
#' birds <- dplyr::filter(class == "Aves")
#' # Subset spatial SEINet data
#' sei_birds <- build_seinet_spatial_data(sei_dat, birds)
#'
#' ## End(Not run)
build_seinet_spatial_data <- function(sei_data, spp_list) {

  # sei_data = targets::tar_read(sei_unit)
  # spp_list = targets::tar_read(elig_list)

  var_names = c(
    "taxon_id", "SEINet_taxonID", "occurrenceID", "scientificName", 
    "scientificNameAuthorship", "basisOfRecord", "eventDate", 
    "verbatimEventDate", "institutionCode", "collectionCode", "collectionID", 
    "recordedBy", "identifiedBy", "occurrenceRemarks", "habitat",  "references",
    "country", "stateProvince", "county", "locality",
    "geodeticDatum", "coordinateUncertaintyInMeters",
    "georeferencedBy", "georeferenceProtocol", "georeferenceProtocol",
    "georeferenceSources", "georeferenceRemarks",
    "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet",
    "infraspecificEpithet", "taxonRank"
  )

  # Filter spatial data
  elig_sei = sei_data |>
    dplyr::select(dplyr::any_of(var_names)) |>
    dplyr::filter(taxon_id %in% spp_list$taxon_id)

  return(elig_sei)
}


#' Read SEINet data into R
#'
#' This function reads SEINet data in Darwin Core Archive format into R and
#'     pulls taxon ID's and taxonomies from GBIF using `get_taxonomies()`
#'     function.
#'
#' @param dir_path Path to SEINet data folder.
#' @param crs Target coordinate reference system (CRS). Either and
#'                [sf::st_crs()] object or accepted input string (e.g. "NAD83").
#'                See [sf::st_crs()] for more details. Default is NULL. If NULL,
#'                resulting sf object CRS will be WGS84.
#'
#' @return An `sf` class object.
#'
#' @seealso [get_taxonomies()]
#'
#' @details
#' SEINet data are available through an online data portal and need to be
#'     downloaded manually. A polygon box, or well-known text (WKT) footprint,
#'     is drawn around an area of interest to query species observations using
#'     the Taxonomic Criteria search page
#'     (<https://swbiodiversity.org/seinet/collections/harvestparams.php>).
#'     The query results need to be manually downloaded in Darwin Core Archive
#'     format and unzipped before using this function.
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#'
#' # Path to data folder
#' data_folder <- file.path("T:/path/to/project/directory", "data/SEINet")
#'
#' # Pull data from existing SEINet query
#' sei_dat <- get_seinet_data(data_folder, crs = "EPSG:26913")
#'
#' ## End(Not run)
get_seinet_data <- function(dir_path, crs = NULL){
  # dir_path = file.path("data", "SEINet"); crs = "EPSG:26913"

  data_path = file.path(dir_path, "occurrences.csv")
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS",
                   "ymd", "ymd HM")

  #  Read data into R
  raw_dat = readr::read_csv(data_path, show_col_types = FALSE) |>
    dplyr::filter(taxonRank %in% c('Species', 'Variety', 'Subspecies')) |>
    dplyr::filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) |>
    dplyr::mutate(
      date = lubridate::parse_date_time(eventDate, date_formats) |> as.Date(),
      date = ifelse(lubridate::year(date) == 9999, NA, date),
      dayOfYear = lubridate::yday(date),
      # year = lubridate::year(date),
      source = "SEINet"
      ) |>
    dplyr::rename("SEINet_taxonID" = taxonID) |>
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
                 crs = "EPSG:4326")
  # Get taxonomies
  gbif_tids = sf::st_drop_geometry(raw_dat) |>
    dplyr::select(SEINet_taxonID, scientificName) |>
    dplyr::distinct() |>
    mpsgSE::get_taxonomies(query_field = "scientificName") |>
    dplyr::select(-scientificName)

  # Rename SEINet taxonomy variables that are duplicated in GBIF
  gbif_vars = colnames(gbif_tids)[!colnames(gbif_tids) %in%
                                    c("scientificName", "SEINet_taxonID")]
  sei_vars = colnames(raw_dat)[colnames(raw_dat) %in% gbif_vars]
  new_vars = paste0("SEINet_", sei_vars)
  colnames(raw_dat)[colnames(raw_dat) %in% sei_vars] = new_vars

  sei_data = dplyr::left_join(raw_dat, gbif_tids,
                              by = "SEINet_taxonID",
                              relationship = 'many-to-many')
  # Re-project CRS
  if(!is.null(crs)){
    if(sf::st_crs(sei_data) != crs){
      sei_data = sf::st_transform(sei_data, crs = crs)
      }
  }

  return(sei_data)
}

#' Summarize SEINet data by species
#'
#' This function summarizes the spatial SEINet object from `get_seinet_data()`
#'     by species. The summary includes the number of records per species,
#'     minimum and maximum year a species is observed, and the SEINet occurrence ID if
#'     there are less than seven (7) observations.
#'
#' @param seinet_data Spatial SEINet data from `get_seinet_data()`.
#' @param locale Logical. Location description of data. E.g., unit acronym or "Buffer"
#'
#' @return A [tibble::tibble()].
#'
#' @seealso [get_seinet_data()], [get_taxonomies()]
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#'
#' # Path to data folder
#' data_folder <- file.path("T:/path/to/project/directory", "data/SEINet")
#'
#' # Pull data from existing SEINet query
#' sei_dat <- get_seinet_data(data_folder, crs = "NAD83")
#'
#' # Summarize species
#' spp_list <- build_seinet_spp(sei_dat)
#'
#' ## End(Not run)
build_seinet_spp <- function(seinet_data, locale = TRUE){
  
  # Date formats
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS",
                   "ymd", "ymd HM")
  taxa_select = c("taxon_id", "kingdom", "phylum", "class", "order", "family",
                  "genus", "species", "subspecies", "variety", "form")

  # Locale
  if(isTRUE(locale)){
    locale = stringr::str_c(unique(seinet_data$locale), collapse = ", ")
  }

  # Subset species names
  t_ids = sf::st_drop_geometry(seinet_data) |>
    dplyr::select(taxon_id, SEINet_taxonID, scientificName) |>
    dplyr::filter(!taxon_id == "" | !is.na(taxon_id)) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_taxon = ifelse(duplicated(taxon_id) |
                                       duplicated(taxon_id, fromLast = TRUE),
                                     "Yes", "No"))
  # Summarize data
  t_id_sum = sf::st_drop_geometry(seinet_data) |>
    dplyr::select(taxon_id, occurrenceID, eventDate) |>
    dplyr::mutate(
      date = lubridate::parse_date_time(eventDate, date_formats) |> as.Date(),
      year = lubridate::year(date)
    ) |>
    dplyr::group_by(taxon_id) |>
    dplyr::summarize(
      nObs = dplyr::n(),
      minYear = min(year, na.rm = TRUE),
      maxYear = max(year, na.rm = TRUE),
      occID = ifelse(nObs <= 6,
                     stringr::str_c(unique(occurrenceID), collapse = ", "),
                     NA),
      .groups = "drop"
      ) |>
    dplyr::mutate(locale = locale, source = "SEINet")
  # Subset taxonomy
  taxa = sf::st_drop_geometry(seinet_data) |>
    dplyr::select(dplyr::any_of(taxa_select)) |>
    dplyr::distinct()


  dat = dplyr::left_join(t_ids, t_id_sum, by = "taxon_id",
                         relationship = 'many-to-many')|>
    dplyr::left_join(taxa, by = 'taxon_id', relationship = 'many-to-many') |>
    dplyr::arrange(kingdom, phylum, class, order, family, genus,
                   species, scientificName) |>
    dplyr::rename("scientific_name" = scientificName)
  return(dat)
}
