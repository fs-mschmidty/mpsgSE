#' Build IUCN Maps
#' 
#' This function reads spatial (polygon) range data from International Union for 
#'     Conservation of Nature (IUCN) into R. These shapefiles were downloaded 
#'     from IUCN (<https://www.iucnredlist.org/resources/spatial-data-download>), 
#'     and saved on the MPSG T-drive. This function will this function will fail 
#'     if you are not on a FS network or in the FS VDI. Use 
#'     `get_iucn_shp_paths()` to get the shapefile paths prior to running this 
#'     function.
#'
#' @param iucn_paths 
#' 
#' @seealso [get_taxonomies()], [get_iucn_shp_paths()]
#' 
#' @return An [sf] object.
#' 
#' @export
#' 
#' @examples
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' bien_map_paths <- get_iucn_shp_paths(spp_list)
#' bien_maps <- build_iucn_maps(bien_map_paths)
build_iucn_maps <- function(iucn_paths) {
  grouped_data = iucn_paths |>
    dplyr::group_by(file_path) |>
    dplyr::summarize(
      where_in = paste(sprintf("'%s'", iucn_name), collapse = ", ")
                     ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      layer_n = basename(file_path) |> stringr::str_replace(".shp", "")
      )

  test_f = function(file_path, where_in, layer_n) {
    query_string = sprintf("SELECT * FROM %s WHERE sci_name IN (%s)", 
                           layer_n, where_in)
    sf::st_read(file_path, query = query_string)
  }
  
  purrr::pmap(grouped_data, test_f) |>
    dplyr::bind_rows() |>
    dplyr::left_join(dplyr::select(iucn_paths, taxon_id, iucn_name), 
                     by = c("sci_name" = "iucn_name"))
}


#' Get paths to IUCN Shapefiles
#' 
#' This function reads in an *.RDS file of IUCN shapefile paths and filters it 
#'     to the eligible species list. The *.RDS file is on the MPSG T-drive, so 
#'     this function will fail if you are not on a FS network or in the FS VDI.
#'
#' @param eligible_list A species list with taxon ID's from `get_taxonomies()`.
#'
#' @seealso [get_taxonomies()]
#' 
#' @return A [tibble::tibble()].
#' 
#' @export
#' 
#' @examples
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' bien_map_paths <- get_iucn_shp_paths(spp_list)
get_iucn_shp_paths <- function(eligible_list){

  # Read in RDS file and filter to eligible list
  iucn_tids = readRDS(file.path("T:/FS/NFS/PSO/MPSG/Data/ExternalData/IUCN",
                                "iucn_tid_paths"))
  # Filter by 'taxon_id'
  iucn_paths = dplyr::filter(iucn_tids, taxon_id %in% eligible_list$taxon_id)
  return(iucn_paths)
}