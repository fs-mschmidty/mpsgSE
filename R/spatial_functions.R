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
#' @return An [sf] object
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


#' Get base map data
#' 
#' @description
#' Pull spatial base map data for North and South America, the lower 48 US 
#'     states, and a user specified National Forest. Continental and national 
#'     scale data are acquired using the [rnaturalearth] package and Forest 
#'     Service data are acquired from Forest Service EDW Rest Services
#'     (https://apps.fs.usda.gov/arcx/rest/services/EDW) using [arcgislayers] 
#'     package. Roads data are acquired using the [osmdata] package.
#'
#' @param states A list of state names or abbreviations.
#' @param region_number The Forest Service Region number
#' @param forest_number The Forest Service Forest number.
#' @param forest_name The Name of the National Forest.
#' @param crs The target coordinate reference system. The default is EPSG:26912
#'                (NAD83 UTM Zone 12).
#'
#' @details
#' `get_basemap_data` returns a list of spatial features used to produce 
#'     automated species evaluations.
#' @details
#' Continental-scale data include 'americas', 'north_america', and 'l_48'. These 
#'     data are acquired using [rnaturalearth::ne_countries()] and 
#'     [rnaturalearth::ne_states()] functions. These data are in the NAD83 CONUS 
#'     Albers (EPSG:5070).
#' @details
#' National Forest-scale data include 'admin_bndry', 'plan_area', 'districts', 
#'     'buffer', and 'aoa_bbox.' 'admin_bndry', 'plan_area', and 'districts' are 
#'     acquired using [read_edw_lyr()]. 'buffer' and 'aoa_bbox' are derived 
#'     using the [sf] package. These data are returned in the coordinate 
#'     reference system provided by the `crs` parameter.
#' @details
#' Roads data include 'roads'. These data are acquired using the [osmdata] 
#'     package, primarily [osmdata::getbb()], [osmdata::opq()], and 
#'     [osmdata::osmdata_sf()]. These data are returned in the coordinate 
#'     reference system provided by the `crs` parameter.
#' @note
#' Sometimes the connection to the Forest Service REST Service or [osmdata] 
#'     fails. This will throw the following error message: "**Error:** 
#'     tar_make() Status code: 500. Error: json". In most instances, the 
#'     function can simply be executed again to retrieve the data. Check the 
#'     data servers if the error is persistent. 
#' @returns A list of [sf] objects.
#' @seealso [read_edw_lyr()], [rnaturalearth::ne_countries()], 
#'          [rnaturalearth::ne_states], [osmdata::osmdata_sf()]
#' @export
#'
#' @examples
#' library(mpsgSE)
#' states <- c("Utah", "Nevada", "New Mexico")
#' region_number <- "04"
#' forest_number <- "07"
#' forest_name <- "Dixie National Forest"
#' basemap_data <- get_basemap_data(states, region_number, forest_number, 
#'                                  forest_name)
get_basemap_data = function(states, region_number, forest_number, forest_name,
                            crs = "EPSG:26912"){
  
  message("Western hemisphere")
  americas = rnaturalearth::ne_countries(scale = "medium",
                                         continent = c("North America",
                                                       "South America"),
                                         returnclass = "sf") |>
    dplyr::filter(name != "Hawaii") |>
    sf::st_transform(crs = 5070)
  
  message("North America")
  north_america_c = rnaturalearth::ne_countries(
    scale = "medium", continent = "North America", returnclass = "sf"
  ) |>
    dplyr::select(name) |>
    dplyr::filter(name != "United States of America")
  north_america_s = rnaturalearth::ne_states(
    country = c("United States of America", "Canada", "Mexico"), 
    returnclass = "sf"
  ) |>
    dplyr::filter(name != "Hawaii") |>
    dplyr::select(name = name_en)
  north_america = dplyr::bind_rows(north_america_c, north_america_s) |>
    sf::st_transform(crs = 5070)
  
  message("Lower 48 states")
  l_48 = rnaturalearth::ne_states(country = c("United States of America")) |>
    sf::st_as_sf() |>
    dplyr::filter(name != "Hawaii", name != "Alaska") |>
    sf::st_transform(crs = 5070)
  
  message("FS Boundaries")
  # Administrative Boundary
  admin_bndry = read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |> 
    dplyr::filter(region == region_number & forestnumber == forest_number) |>
    sf::st_transform(crs) 
  # Plan Area (Forest Service Land)
  plan_area = read_edw_lyr("EDW_BasicOwnership_02") |> 
    dplyr::filter(region == region_number & forestname == forest_name) |>
    dplyr::filter(ownerclassification != "NON-FS") |>
    sf::st_transform(crs) 
  # Ranger Districts
  districts = read_edw_lyr("EDW_RangerDistricts_03", layer = 1) |> 
    dplyr::filter(region == region_number & forestnumber == forest_number) |>
    sf::st_transform(crs) 
  # Buffer
  buffer = sf::st_buffer(admin_bndry, 1000) |> sf::st_difference(admin_bndry) |>
    sf::st_transform(crs) 
  # Area of Analysis
  aoa_bbox = buffer |> sf::st_bbox() |>
    sf::st_transform(crs) 

  #-- Roads
  message("Roads")
  # Area of Analysis
  aoa = sf::st_buffer(admin_bndry, 1000000) |> sf::st_bbox()
  # Roads
  roads = lapply(states, function(state){
    osm_dat = osmdata::getbb(state) |> 
      osmdata::opq() |>
      osmdata::add_osm_feature("highway", 
                               value = c("motorway", "trunk", "primary")) |>
      osmdata::osmdata_sf()
    return(osm_dat$osm_lines)
  }) |> 
    dplyr::bind_rows() |>
    sf::st_transform(crs) |> 
    sf::st_crop(aoa) |> 
    suppressWarnings()
  
  dat = tibble::lst(americas, north_america, l_48, admin_bndry, plan_area, 
                    districts, buffer, aoa_bbox, roads)
  return(dat)
}


#' Read spatial data from Forest Service ArcGIS REST Services
#' 
#' read_edw_lyr reads features from the Forest Service ArcGIS REST Services 
#'     Directory, https://apps.fs.usda.gov/arcx/rest/services/EDW, using the 
#'     [arcgislayers] package.
#'     
#'
#' @param map_name Character. Name of map layer.
#' @param layer Integer. Number of layer to read. Default is  zero (0).
#' @param crs Coordinate reference system (crs). Default is EPSG:26912 (NAD83 
#'                UTM Zone 12).
#'
#' @return An [sf] object or [terra::SpatRaster-class].
#' @seealso [arcgislayers::arc_read()], [sf::st_transform()]
#' @export
#' 
#' @examples
#' library(mpsgSE)
#' 
#' # Administrative Boundary for the Dixie National Forest
#' admin_bndry <- read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |> 
#'   dplyr::filter(forestname == "Dixie National Forest")
#' 
read_edw_lyr <- function(map_name, layer = 0, crs = "EPSG:26912"){
  # map_name = "EDW_ForestSystemBoundaries_01"
  # layer = 1
  edw_rest <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/"
  lyr = arcgislayers::arc_read(
    glue::glue(edw_rest, "{map_name}/MapServer/{layer}")
  ) |>
    janitor::clean_names() |> 
    sf::st_transform(crs)
  return(lyr)
}


#' Read feature class into R.
#' 
#' This function uses the `sf` package to read a feature class into R from a 
#'   geodatabase (*.gdb) using the `sf::read_sf()` function. It then checks that 
#'   the feature class is in the target coordinate reference system (CRS) and 
#'   will transform the feature to the target CRS if it is not.
#'
#' @param lyr Feature class name.
#' @param dsn Path to geodatabase that holds `lyr`.
#' @param crs Target coordinate reference system (CRS). Either and 
#'   `sf::st_crs()` object or accepted input string for `sf::st_crs()` (e.g. 
#'   "WGS84" or "NAD83"). See [sf::st_crs()] for more details. Default is NULL. 
#'   If NULL, resulting [sf] object will not be transformed.
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
  fc = sf::read_sf(layer = lyr, dsn = dsn) |> sf::st_make_valid()
  if(!is.null(crs)){fc = sf::st_transform(fc, crs = crs)}
  return(fc)
}

