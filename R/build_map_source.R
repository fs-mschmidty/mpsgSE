#' Build Map Sources
#' 
#' This function takes in the maps that were retrieved using 
#'     `download_bien_range_maps()`,`download_ebird_range_maps()`, and 
#'     `build_iucn_maps()` and returns the data frame with taxonomic ID's, 
#'     taxonomies, and range map data sources.
#'     
#' @param spp_list A species list with taxon ID's from `get_taxonomies()`.
#' @param bien_maps `sf` object from `download_bien_range_maps()`.
#' @param ebird_maps `sf` object from `download_ebird_range_maps()`.
#' @param iucn_maps `sf` object from `build_iucn_maps()`.
#' 
#' @return A [tibble::tibble()]
#' 
#' @seealso [get_taxonomies()], 
#' 
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' ebird_key <- "abcde12fghij34"
#' # Build speceis list
#' spp_list <- get_taxonomies(sp_list_ex)
#' 
#' # BIEN Maps
#' bien_range_maps <- download_bien_range_maps(spp_list, 
#'                                             file.path("data", "bien_maps"))
#' 
#' # eBird Maps
#' ebird_status <- download_ebird_status_maps(spp_list, 
#'                                            output_path = file.path("data/ebirdst"), 
#'                                            ebird_access_key = ebird_key)
#' ebird_maps <- download_ebird_range_maps(spp_list,
#'                                         output_path = file.path("data/ebirdst"),
#'                                         ebird_access_key = ebird_key)
#' 
#' # IUCN Maps
#' map_paths <- get_iucn_shp_paths(spp_list)
#' iucn_maps <- build_iucn_maps(map_paths)
#'
#' # Build map source data frame
#' map_sources <- build_map_source(spp_list, bien_maps, ebird_maps, iucn_maps)
#' 
#' ## End(Not run)                     
build_map_source <- function(spp_list, bien_maps, ebird_maps, iucn_maps) {
  all_maps = dplyr::bind_rows(
    sf::st_drop_geometry(iucn_maps) |> dplyr::select(taxon_id) |> 
      dplyr::mutate(taxon_id = as.numeric(taxon_id), source = "IUCN"),
    dplyr::select(bien_maps, taxon_id) |> 
      dplyr::mutate(taxon_id = as.numeric(taxon_id), source = "BIEN"),
    sf::st_drop_geometry(ebird_maps)|> dplyr::select(taxon_id) |> 
      dplyr::distinct() |> 
      dplyr::mutate(taxon_id = as.numeric(taxon_id), source = "eBird")
    )
  source_list = spp_list |>
    dplyr::select(taxon_id, scientific_name, common_name, kingdom:genus) |>
    dplyr::left_join(all_maps, by = "taxon_id", relationship = 'many-to-many') |> 
    dplyr::distinct()
  return(source_list)
}
