#' Read feature class into R.
#' 
#' This function uses the `sf` package to read a feature class into R from a 
#'   geodatabase using the `sf::read_sf()` function. It then checks that the 
#'   feature class is in the target coordinate reference system (CRS) and will 
#'   transform the feature to the target CRS if it is not.
#'
#' @param lyr Feature class name.
#' @param dsn Path to geodatabase that holds the feature class.
#' @param crs Target coordinate reference system (CRS). Either and 
#'   `sf::st_crs()` object or accepted input string for `sf::st_crs()` (e.g. 
#'   "WGS84" or "NAD83"). See `sf::st_crs()` for more details. Default is NULL. 
#'   If NULL, resulting sf object will not be transformed.
#'
#' @return sf object
#' @seealso [sf::read_sf()], [sf::st_crs()]
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' read_fc(lyr = "feature_name", dsn = file.path("T:/path/to/geodatabase"), 
#'         crs = "NAD83")
#' 
#' ## End (Not run)
read_fc <- function(lyr, dsn, crs = NULL){
  fc = sf::read_sf(layer = lyr, dsn = dsn) |> 
    sf::st_make_valid()
  if(!is.null(crs)){fc = sf::st_transform(fc, crs = crs)}
  return(fc)
}


#' Clip feature class to polygon
#' 
#' This function clips a `sf` object using `sf::st_intersection()`. First, this 
#'   function checks that the coordinate reference system (CRS) of the input 
#'   object is the same as the clipping object. If it is not, this function
#'   transforms the clipping object to CRS of the input object using
#'   `sf::st_transform()`before clipping. The output CRS is not changed.
#'
#' @param sf_lyr  Spatial (`sf`) object to be clipped.
#' @param sf_clip Polygon (`sf`) object used to clip.
#' @param locale  Optional. Short description of clipped layer, usually the 
#'                    location (e.g., forest acronym or "Buffer").
#'
#' @return sf object
#' @seealso [sf::st_intersection()], [sf::st_transform()]
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
#' sf_plan_area <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' 
#' # Pull data from existing GBIF query
#' gbif_dat <- get_gbif(gbif_key = '9999999-999999999999999', 
#'                      t_path = file.path(t_path, "data"))
#' 
#' # Convert to spatial object
#' gbif_sf <- gbif_spatial(gbif_dat, "NAD83")
#' 
#' # Clip to extents
#' unit_gbif <- clip_fc(gbif_sf, sf_plan_area)
#' 
#' ## End (Not run)
clip_fc <- function(sf_lyr, sf_clip, locale = NULL){
  
  # Transform clipping layer
  if(sf::st_crs(sf_lyr) != sf::st_crs(sf_clip)){
    sf_clip = sf::st_transform(sf_clip, crs = sf::st_crs(sf_lyr))
  }
  
  # Clip input layer
  sf_lyr = sf::st_intersection(sf_lyr, sf_clip) |> 
    dplyr::select(-tidyselect::any_of(colnames(sf_clip)))
  
  # Add locale
  if(!is.null(locale)){
    sf_lyr = dplyr::mutate(sf_lyr, locale = locale)
  }
  
  return(sf_lyr)
}
