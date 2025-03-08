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
#'   transforms the input object to CRS of the clipping object using
#'   `sf::st_transform()`before clipping. The output CRS is that of the clipping 
#'   object.
#'
#' @param sf_lyr  Spatial (`sf`) object to be clipped.
#' @param sf_clip Polygon (`sf`) object used to clip by.
#' @param locale  Short description of clipped layer, usually the location 
#'   (e.g., forest acronym or "Buffer").
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
#' 
#' ## End (Not run)
clip_fc <- function(sf_lyr, sf_clip, locale){
  if(sf::st_crs(sf_lyr) != sf::st_crs(sf_clip)){
    sf_lyr = sf::st_transform(sf_lyr, crs = sf::st_crs(sf_clip))
  }
  sf::st_intersection(sf_lyr, sf_clip) |> 
    dplyr::mutate(locale = locale) |> 
    dplyr::select(-tidyselect::any_of(colnames(sf_clip)))
}
