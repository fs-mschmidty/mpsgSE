#' Read SEINet data into R
#' 
#' This function reads SEINet data in Darwin Core Archive format into R.
#'
#' @param dir_path Path to SEINet data folder.
#' @param crs Target coordinate reference system (CRS). Either and 
#'                `sf::st_crs()` object or accepted input string (e.g. "NAD83"). 
#'                See `sf::st_crs()` for more details. Default is NULL. If NULL, 
#'                resulting sf object CRS will be WGS84.
#'
#' @return An sf class object.
#' 
#' @details
#' Additional details...
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
#' sei_dat <- get_seinet(data_folder, crs = "NAD83")
#' 
#' ## End(Not run)                     
get_seinet <- function(dir_path, crs = NULL){
  data_path = file.path(dir_path, "occurrences.csv")
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS", 
                   "ymd", "ymd HM")
  #  Read data into R
  dat = readr::read_csv(data_path, show_col_types = FALSE) |> 
    dplyr::filter(taxonRank %in% c('Species', 'Variety', 'Subspecies')) |> 
    dplyr::filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) |> 
    dplyr::mutate(
      date = lubridate::parse_date_time(eventDate, date_formats) |> as.Date(),
      date = ifelse(lubridate::year(date) == 9999, NA, date),
      dayOfYear = lubridate::yday(date),
      # year = lubridate::year(date),
      source = "SEINet"
      ) |> 
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                 crs = "WGS84")
  # Re-project CRS
  if(!is.null(crs)){
    if(sf::st_crs(dat) != crs) dat = sf::st_transform(dat, crs = crs)
  }
  
  return(dat)
}

#' Summarize SEINet data by species
#' 
#' This function summarizes the spatial SEINet object from `get_seinet()` by 
#'     species. The summary includes the number of records per species, minimum 
#'     and maximum year a species is observed, and the SEINet occurrence ID if 
#'     there are less than seven (7) observations. This function then verifies 
#'     taxonomy using the `get_taxonomies()` function.
#'
#' @param seinet_data Spatial SEINet data from `get_seinet()`.
#'
#' @return A tibble.
#' @seealso [get_seinet()], [get_taxonomies()]
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
#' sei_dat <- get_seinet(data_folder, crs = "NAD83")
#' 
#' # Summarize species
#' spp_list <- seinet_spp(sei_dat)
#' 
#' ## End(Not run)                     
seinet_spp <- function(seinet_data, locale = TRUE){
  # Date formats
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y", "ymd HMS", 
                   "ymd", "ymd HM")
  # Locale
  if(isTRUE(locale)){
    locale = stringr::str_c(unique(seinet_data$locale), collapse = ", ")
  }
  
  # Summarize data
  dat = sf::st_drop_geometry(seinet_data) |> 
    dplyr::select(taxonID, occurrenceID, scientificName, eventDate) |> 
    dplyr::mutate(
      date = lubridate::parse_date_time(eventDate, date_formats) |> as.Date(),
      year = lubridate::year(date)
    ) |> 
    dplyr::rename("scientific_name" = scientificName) |>
    dplyr::distinct() |> 
    dplyr::group_by(scientific_name) |> 
    dplyr::summarize(nObs = dplyr::n(), 
                     minYear = min(lubridate::year(date), na.rm = TRUE), 
                     maxYear = max(lubridate::year(date), na.rm = TRUE), 
                     taxonID = stringr::str_c(taxonID, collapse = ", "),
                     occID = ifelse(nObs <= 6, 
                                    stringr::str_c(unique(occurrenceID),
                                                   collapse = ", "),
                                    NA), 
                     .groups = "drop") |> 
    dplyr::mutate(locale = locale, source = "SEINet") |> 
    dplyr::filter(!scientific_name == "") |> 
    dplyr::distinct(scientific_name, .keep_all = TRUE) |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name") |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}
