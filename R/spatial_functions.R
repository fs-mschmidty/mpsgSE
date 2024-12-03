#' Read feature class into R.
#' 
#' This function uses the `sf` package to read a feature class into R from a 
#'     geodatabase using the `sf::read_sf` function. It then checks that the 
#'     feature class is in the provided coordinate reference system (CRS) and 
#'     will transform the feature to the provided CRS if it is not using 
#'     `sf::st_transform()`.
#'
#' @param lyr Feature class name.
#' @param dsn Path to geodatabase that holds the feature class.
#' @param crs Target coordinate reference system (CRS). Either and `sf::st_crs()` 
#'                object or accepted input string for `sf::st_crs()`. See 
#'                `sf::st_crs()` for more details.
#'
#' @return sf object
#' @export
#' 
#' @examples
#' ## Not run:
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' read_lyr(lyr = "admin_bdy", dsn = file.path("T:/path/to/geodatabase"), 
#'          crs = "NAD83")
#' 
#' ## End (Not run)
read_fc <- function(lyr, dsn, crs){
  fc = sf::read_sf(layer = lyr, dsn = dsn) |> 
    sf::st_make_valid()
  if(sf::st_crs(fc) != crs) fc = sf::st_transform(fc, crs = crs)
  return(fc)
}




#' Clip feature class to polygon
#' 
#' This function clips a `sf` object using `sf::st_intersection()`. First, this 
#'     function checks that the coordinate reference system (CRS) of the input 
#'     object is the same as the clipping object. If it is not, this function 
#'     transforms the CRS of the input object to the clipping object using  
#'     `sf::st_transform()`before clipping.
#'
#' @param sf_lyr  Spatial (`sf`) object to be clipped.
#' @param sf_clip Polygon (`sf`) object used to clip `sf_lyr` by.
#' @param locale  Character. Short description of clipped layer, usually the 
#'                    location (e.g., forest acronym or "Buffer").
#'
#' @return sf object
#' @export
#' 
#' @examples
#' ## Not run:
#' devtools::install_github("fs-mschmidty/mpsgSE")
#' library("mpsgSE")
#' 
#' admin_bdy_sf <- read_lyr(lyr = "admin_bdy", 
#'                          dsn = file.path("T:/path/to/geodatabase"), 
#'                          crs = "NAD83")
#'                          
#' clip_fc(sf_lyr = gbif_sf, sf_clip = admin_bdy_sf, locale = "FS")
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
