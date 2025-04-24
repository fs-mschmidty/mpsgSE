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
  date_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%Y-%m", "%Y")
  #  Read data into R
  dat = readr::read_csv(data_path, show_col_types = FALSE) |> 
    dplyr::filter(taxonRank %in% c('Species', 'Variety', 'Subspecies')) |> 
    dplyr::filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) |> 
    dplyr::mutate(date = lubridate::parse_date_time(eventDate, 
                                                    orders = date_formats), 
                  date = ifelse(lubridate::year(date) == 9999, NA, date), 
                  dayOfYear = lubridate::yday(date),
                  year = lubridate::year(date), 
                  source = "SEINet") |> 
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
seinet_spp <- function(seinet_data){
  locale = stringr::str_c(unique(seinet_data$locale), collapse = ", ")
  dat = sf::st_drop_geometry(seinet_data) |> 
    dplyr::select(occurrenceID, scientificName, taxonID, date) |> 
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
    dplyr::mutate(SEI_locale = locale, source = "SEINet") |> 
    dplyr::filter(!scientific_name == "") |> 
    dplyr::distinct(scientific_name, .keep_all = TRUE) |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name") |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}


#' Compile SEINet list
#' 
#' Compile a comprehensive species list from SEINet occurrence records on Forest 
#'     Service (FS) land and a 1-km buffer of FS land. This function uses the 
#'     `seinet_spp()` function on two clipped spatial objects from 
#'     `get_seinet()`.
#'
#' @param seinet_unit Spatial SEINet data from `get_seinet()` clipped to FS land.
#' @param seinet_buff Spatial SEINet data from `get_seinet()` clipped to the 1-km 
#'                    buffer of FS land.
#'
#' @return A tibble.
#' @seealso [get_seinet()], [seinet_spp()], [clip_fc()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_fs <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' sf_buff <- read_fc(lyr = "PlanArea_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#' 
#' # Pull data from existing SEINet query
#' sei_dat <- get_seinet(file.path(t_path, "data/SEINet"), crs = "NAD83")
#' 
#' # Clip to extents
#' unit_sei <- clip_fc(sei_dat, sf_fs)
#' buff_sei <- clip_fc(sei_dat, sf_buff)
#' 
#' # Summarize species
#' spp_list <- compile_seinet_list(unit_sei, buff_sei)
#' 
#' ## End(Not run)                     
compile_seinet_list <- function(seinet_unit, seinet_buff){
  message("Processing unit species data")
  unit_list = seinet_spp(seinet_unit)
  message("Processing buffer species data")
  buff_list = seinet_spp(seinet_buff)
  message("Compiling species list")
  comp_list = rbind(add_cols(unit_list, buff_list),
                     dplyr::filter(add_cols(buff_list, unit_list), 
                                   !taxon_id %in% unit_list$taxon_id))
  return(comp_list)
  }

