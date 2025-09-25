#' Download BIEN range maps
#' 
#' This function downloads range maps from the Botanical Information and Ecology 
#'     Network (BIEN; <https://bien.nceas.ucsb.edu/bien/>) API for plants 
#'     identified as eligible for Species of Conservation Concern.
#'
#' @param spp_list List of species with taxonomies from `get_taxonomies()`.
#' @param output_path Directory path to save range map data to.
#' 
#' @seealso [get_taxonomies()]
#'
#' @return A [tibble::tibble()]
#' 
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' bien_range_maps <- download_bien_range_maps(spp_list, file.path("data", "bien_maps"))
#' 
#' ## End(Not run)                     
download_bien_range_maps <- function(spp_list, output_path) {
  # spp_list = targets::tar_read(elig_list)
  # output_path = file.path("data", "bien_maps")
  
  # subset plants
  plants = spp_list |>
    dplyr::filter(kingdom == "Plantae") |>
    dplyr::select(taxon_id, scientific_name) |> 
    dplyr::distinct(taxon_id, .keep_all = TRUE)
  
  # evaluate output directory
  if(!dir.exists(output_path)) dir.create(output_path)
  
  # download range maps
  bien_maps = BIEN::BIEN_ranges_species(plants$scientific_name,
                                        directory = output_path,
                                        matched = TRUE) |> 
    janitor::clean_names() |> 
    dplyr::filter(range_map_downloaded == "Yes") |> 
    dplyr::mutate(species = gsub("_", " ", species)) |> 
    dplyr::rename("scientific_name" = species) |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name")
  
  return(bien_maps)
  }