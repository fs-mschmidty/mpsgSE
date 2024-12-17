#' Query GBIF species occurrence records
#' 
#' This function queries GBIF for species occurrence records for a given area, 
#'   or polygon (`sf` object), and then reads the data into R.
#'
#' @param gbif_key The 22-digit GFIB key including hyphen for the data package. 
#'   Use "new" for new GBIF queries.
#' @param t_path The directory path where the GBIF data package is or will be 
#'   stored.
#' @param aoa_wkt Well-known text (wtk) string for the area of analysis. Use 
#'   `wkt_string()` to generate the wtk string. This is required when *gbif_key* 
#'   is set to **new**. Default is NULL.
#' @param gbif_user Your GBIF user name. This is required when *gbif_key* is set 
#'   to **new**. Default is NULL.
#' @param gbif_pwd Your GBIF password. This is required when *gbif_key* is set 
#'   to **new**. Default is NULL.
#' @param gbif_email Your GBIF email address. This is required when *gbif_key* 
#'   is set to **new**. Default is NULL.
#' @param gbif_format The format of the data returned from GBIF. Default is 
#'   Darwin-Core Achrive (DWAC). See `rgbif::occ_download()` for more details.
#' @param crs Target coordinate reference system (CRS). Either and 
#'                `sf::st_crs()` object or accepted input string for 
#'                `sf::st_crs()` (e.g. "WGS84" or "NAD83"). See `sf::st_crs()`
#'                for more details. Default is NULL. If NULL, resulting sf 
#'                object CRS will be WGS84.
#' @param process_data Logical. Process data after reading them into R (TRUE == 
#'   yes, FALSE == no). Default is TRUE. The processing step  
#'   
#'   1. filters the data for species, subspecies, and varieties,  
#'   2. filters the data for present records,  
#'   3. filters against fossil records,  
#'   4. assembles clean scientific names (i.e., without authority) from the 
#'   genus, specific epithet, and infraspecific epithet, and  
#'   5. attempts to parse dates, day of year, and year values.
#'
#' @return A tibble.
#' @details
#' Additional details...
#' @seealso [rgbif::pred_within()], [rgbif::occ_download()], 
#'          [rgbif::occ_download_wait()], [rgbif::occ_download_get()], 
#'          [rgbif::occ_download_import()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_aoa <- read_fc(lyr = "AdminBdy_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#' 
#' # New GBIF data query
#' gbif_dat <- get_gbif(gbif_key = 'new', 
#'                      t_path = file.path(t_path, "data"), 
#'                      aoa_wkt = wkt_string(sf_aoa),
#'                      gbif_user = Sys.getenv("GBIF_USER"),
#'                      gbif_pwd = Sys.getenv("GBIF_PWD"),
#'                      gbif_email = Sys.getenv("GBIF_EMAIL"), 
#'                      crs = 'NAD83')
#'                      
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"), 
#'                      crs = 'NAD83')
#' 
#' ## End(Not run)                     

get_gbif <- function(gbif_key, t_path, aoa_wkt = NULL, gbif_user = NULL, 
                     gbif_pwd = NULL, gbif_email = NULL, gbif_format = "DWCA", 
                     crs = NULL, process_data = TRUE){
  #-- Function variables
  # GBIF data package file path
  gbif_path = if(!gbif_key == "new"){
    file.path(t_path, paste0(gbif_key, ".zip"))
  } else(NULL)
  # Date formats
  date_formats = c("%Y-%m-%d", "%y-%m-%d", "%y-%m", "%y")
  
  #-- Pull GBIF Data
  if(gbif_key == "new"){
    message("Requesting data from GBIF")
    gbifPred = rgbif::pred_within(aoa_wkt)
    gbifDwnld = rgbif::occ_download(gbifPred, user = gbif_user, pwd = gbif_pwd, 
                                     email = gbif_email, format = gbif_format)
    rgbif::occ_download_wait(gbifDwnld)
    gbif = rgbif::occ_download_get(gbifDwnld, path = file.path(t_path)) |>
      rgbif::occ_download_import()
  } else if(file.exists(gbif_path)){
    message("Reading GBIF data into R")
    gbif = rgbif::occ_download_import(key = gbif_key, path = file.path(t_path))
  } else({
    message("Downloading GBIF data")
    gbif = rgbif::occ_download_get(key = gbif_key, path = file.path(t_path)) |>
      rgbif::occ_download_import()
  })
  
  #-- Process GBIF data
  if(isTRUE(process_data)){
    gbif = gbif |>
      # Filter for species & subspecies and not fossil records
      dplyr::filter(taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY") &
                      occurrenceStatus == "PRESENT" &
                      !basisOfRecord == "FOSSIL_SPECIMEN") |>
      # Create clean scientific names
      dplyr::mutate(
        infraspecificEpithet = ifelse(grepl("^//s*$", infraspecificEpithet),
                                      NA, infraspecificEpithet), 
        scientific_name = paste(trimws(genus), trimws(specificEpithet), 
                                sep = " "),
        scientific_name = ifelse(!is.na(infraspecificEpithet),
                                 paste(scientific_name,
                                       trimws(infraspecificEpithet), sep = " "),
                                 scientific_name), 
        scientific_name = trimws(scientific_name),
        # Parse date formats, day of year, and year
        date = lubridate::parse_date_time(eventDate, date_formats),
        dayOfYear = lubridate::yday(date),
        year = lubridate::year(date), 
        source = "GBIF"
      ) |>
      dplyr::mutate_if(is.character, trimws)
  }
  
  gbif = gbif_spatial(gbif)
  
  #-- Return GBIF data
  return(gbif)
}


#' Create a well-known text string (WTK) string
#' 
#' Creates a well-known text string from a polygon (`sf` object).
#'
#' @param my_polygon An `sf` polygon object.
#' 
#' @return A vector.
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_aoa <- read_fc(lyr = "AdminBdy_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#' 
#' # Create WKT string
#' wkt_string(sf_aoa)
#' 
#' ## End (Not run)
wkt_string <- function(my_polygon){
  sf::st_bbox(my_polygon) |> 
    sf::st_as_sfc() |> 
    sf::st_as_text()
}



#' Convert GBIF data frame to an sf object
#' 
#' This function converts the output from `get_gbif()` to a spatial (`sf`) 
#'     object using `sf::st_as_sf()`. This function will also transform the data
#'     to a target coordinate reference system.
#'
#' @param gbif_dat GBIF data frame from `get_gbif()`.
#' @param crs Target coordinate reference system (CRS). Either and 
#'                `sf::st_crs()` object or accepted input string for 
#'                `sf::st_crs()` (e.g. "WGS84" or "NAD83"). See `sf::st_crs()`
#'                for more details. Default is NULL. If NULL, resulting sf 
#'                object CRS will be WGS84.
#'
#' @return An sf object.
#' @seealso [get_gbif()], [sf::st_as_sf()], [sf::st_crs()]
#'
#' @examples
#' ## Not run:
#' 
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' # Project directory path
#' t_path <- file.path("T:/path/to/project/directory")
#' 
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' # Convert to spatial object
#' gbif_sf <- gbif_spatial(gbif_dat, "NAD83")
#' 
#' ## End(Not run)                     
gbif_spatial <- function(gbif_dat, crs = NULL){
  fc = sf::st_as_sf(gbif_dat, coords = c("decimalLongitude", "decimalLatitude"),
                    crs = "WGS84")
  if(!is.null(crs)){
    if(sf::st_crs(fc) != crs) fc = sf::st_transform(fc, crs = crs)
  }
  return(fc)
} 


#' Summarize GBIF data by species
#' 
#' This function summarizes the spatial GBIF object from `gbif_spatial()` by 
#'     species. Currently this function only works when 
#'     `get_gbif(..., process_data = TRUE)`. The summary includes the number of 
#'     records per species, minimum and maximum year a species is observed, and 
#'     the GBIF occurrence ID if there are less than seven (7) observations. 
#'     This function then verifies taxonomy using the `get_taxonomies()` 
#'     function.
#'
#' @param sf_data Spatial GBIF data from `gbif_spatial()`.
#'
#' @return A tibble.
#' @seealso [get_gbif()], [gbif_spatial()], [get_taxonomies()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' # Project directory path
#' t_path <- file.path("T:/path/to/project/directory")
#' 
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' # Convert to spatial object
#' gbif_sf <- gbif_spatial(gbif_dat, "NAD83")
#' 
#' # Summarize species
#' gbif_list <- gbif_spp(gbif_sf)
#' 
#' ## End(Not run)                     
gbif_spp <- function(sf_data){
  locale = stringr::str_c(unique(sf_data$locale), collapse = ", ")
  dat = sf::st_drop_geometry(sf_data) |>
    dplyr::select(scientific_name, taxonKey, occurrenceID, year) |> 
    dplyr::distinct() |>
    dplyr::group_by(scientific_name) |> 
    dplyr::summarize(GBIF_nObs = dplyr::n(), 
                     GBIF_minYear = min(year, na.rm = TRUE), 
                     GBIF_maxYear = max(year, na.rm = TRUE), 
                     GBIF_occID = ifelse(GBIF_nObs <= 6, 
                                         stringr::str_c(unique(occurrenceID), 
                                                        collapse = "; "),
                                         NA),
                     .groups = "drop") |> 
    dplyr::mutate(GBIF_locale = locale, source = "GBIF") |> 
    dplyr::distinct(scientific_name, .keep_all = TRUE) |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name") |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}


#' Compile GBIF list
#' 
#' Compile a comprehensive species list from GBIF occurrence records on Forest 
#'     Service (FS) land and a 1-km buffer of FS land. This function uses the 
#'     `gbif_spp()` function on two clipped spatial objects from 
#'     `gbif_spatial()`.
#'
#' @param unit_sf Spatial GBIF data from `gbif_spatial()` clipped to FS land.
#' @param buff_sf Spatial GBIF data from `gbif_spatial()` clipped to the 1-km 
#'                    buffer of FS land.
#'
#' @return A tibble.
#' @seealso [get_gbif()], [gbif_spatial()], [gbif_spp()], [clip_fc()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_fs <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' sf_buff <- read_fc(lyr = "PlanArea_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#' 
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' # Convert to spatial object
#' gbif_sf <- gbif_spatial(gbif_dat, "NAD83")
#' 
#' # Clip to extents
#' unit_gbif <- clip_fc(gbif_sf, sf_fs)
#' buff_gbif <- clip_fc(gbif_sf, sf_buff)
#' 
#' # Summarize species
#' gbif_list <- compile_gbif_list(unit_gbif, buff_gbif)
#' 
#' ## End(Not run)                     
compile_gbif_list <- function(unit_sf, buff_sf){
  message("Processing unit species data")
  unit_list = gbif_spp(unit_sf)
  message("Processing buffer species data")
  buff_list = gbif_spp(buff_sf)
  message("Compiling species list")
  comp_list = rbind(add_cols(unit_list, buff_list),
                     dplyr::filter(add_cols(buff_list, unit_list), 
                                   !taxon_id %in% unit_list$taxon_id))
  return(comp_list)
}

