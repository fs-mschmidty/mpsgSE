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
#' @return sf objectread_fc <- function(lyr, dsn, crs){
#' @export
#' 
#' @examples
#' ## Not run:
#' # Read a feature class into R
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