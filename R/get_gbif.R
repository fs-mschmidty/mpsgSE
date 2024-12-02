#' Query GBIF species occurrence records
#'
#' @param gbif_key The 22-digit GFIB key including hyphen for the data set. Use 
#'                     "new" for new GBIF queries.
#' @param t_path The directory path where the GBIF data package is or will be 
#'                   stored.
#' @param aoa_wkt Well-known text (wtk) string for the area of analysis. Use 
#'                    `wkt_string` to generate the wtk string. This is required 
#'                    when `gbif_key` is set to **new**. Default is NULL.
#' @param gbif_user Your GBIF user name. This is required when `gbif_key` is set 
#'                      to **new**. Default is NULL.
#' @param gbif_pwd Your GBIF password. This is required when `gbif_key` is set 
#'                     to **new**. Default is NULL.
#' @param gbif_email Your GBIF email address. This is required when `gbif_key` 
#'                       is set to **new**. Default is NULL.
#' @param gbif_format The format of the data returned from GBIF. Default is 
#'                        Darwin-Core Achrive (DWAC). See `rgbif::occ_download` 
#'                        for more details.
#' @param process_data Logical (TRUE/FALSE). Are data to be processed after 
#'                         reading them into R. Default is TRUE. The processing 
#'                         step 1) filters the data for species, subspecies, and 
#'                         varieties, 2) filters the data for present records, 
#'                         3) filters against fossil records, 4) assembles clean 
#'                         scientific names, without authority, from the genus, 
#'                         specific epithet, and infraspecific epithet 
#'                         variables, and 5) attempts to parse dates, day of 
#'                         year, and year values.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' ## Not run:
#' remotes::install_github("fs-mschmidty/mpsgSE")
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
#'                      gbif_email = Sys.getenv("GBIF_EMAIL"))
#'                      
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' ## End(Not run)                     

get_gbif <- function(gbif_key, t_path, aoa_wkt = NULL, gbif_user = NULL, 
                     gbif_pwd = NULL, gbif_email = NULL, gbif_format = "DWCA", 
                     process_data = TRUE){
  #-- Function variables
  # GBIF data package file path
  gbif_path <- if(!gbif_key == "new"){
    file.path(t_path, paste0(gbif_key, ".zip"))
  } else(NULL)
  # Date formats
  date_formats <- c("%Y-%m-%d", "%y-%m-%d", "%y-%m", "%y")
  
  #-- Pull GBIF Data
  if(gbif_key == "new"){
    message("Requesting data from GBIF")
    gbifPred <- rgbif::pred_within(aoa_wkt)
    gbifDwnld <- rgbif::occ_download(gbifPred, user = gbif_user, pwd = gbif_pwd, 
                                     email = gbif_email, format = gbif_format)
    rgbif::occ_download_wait(gbifDwnld)
    gbif <- rgbif::occ_download_get(gbifDwnld, 
                                    path = file.path(t_path)) |>
      rgbif::occ_download_import()
  } else if(file.exists(gbif_path)){
    message("Reading GBIF data into R")
    gbif <- rgbif::occ_download_import(key = gbif_key,
                                       path = file.path(t_path))
  } else({
    message("Downloading GBIF data")
    gbif <- rgbif::occ_download_get(key = gbif_key,
                                    path = file.path(t_path)) |>
      rgbif::occ_download_import()
  })
  
  #-- Process GBIF data
  if(isTRUE(process_data)){
    gbif <- gbif |>
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
        year = lubridate::year(date)
      ) |>
      dplyr::mutate_if(is.character, trimws)
  }
  #-- Return GBIF data
  return(gbif)
}
