#' Get Taxonomies from GBIF
#' 
#' This function adds taxonomy information from GBIF to any data frame that has 
#'     valid scientific names and returns a tibble. `gbif_taxonID` is the GBIF 
#'     ID for the given scientific name and full taxonomy from the GBIF backbone 
#'     taxonomies database. `taxon_id` is ID number of the accepted taxonomy
#'     from the GBIF backbone.
#'
#' @param spp_list A data frame containing valid scientific species names.
#' @param query_field The name of the variable with valid scientific names.
#'
#' @returns A [tibble::tibble()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' 
#' ## End(Not run)                     
get_taxonomies <- function(spp_list, query_field = "scientific_name") {
  # Get list of distinct species.
  distinct_spp = spp_list |>
    dplyr::select(dplyr::any_of(query_field)) |> 
    dplyr::distinct()
  # Clean text
  distinct_spp$my_clean_query_name = distinct_spp |> 
    dplyr::pull(query_field) |>
    stringr::str_replace("[\r\n]", " ") |>
    stringr::str_replace("[\r\n]", "") |>
    stringr::str_replace("  ", " ") |> 
    stringr::str_to_sentence()
  # Get GBIF Taxon ID's
  distinct_spp$gbif_taxonID <- taxize::get_gbifid(
    distinct_spp$my_clean_query_name, ask = FALSE, rows = 1, messages = FALSE
  )
  
  # Pull Taxonomy from GBIF backbone taxonomy
  taxonomy_list = taxize::classification(distinct_spp$gbif_taxonID, db = "gbif")
  
  # Function to convert long list to wide data frame and add taxon ID's
  convert_taxonomy = function(i, tax_list) {
    # Get GBIF IF
    gbif_taxonID = names(tax_list)[[i]]
    if(!is.na(gbif_taxonID)){
      # Get MPSG taxon ID
      final_id = tax_list[[i]] |>
        tail(1) |> 
        dplyr::pull(id)
      # Get taxonomy
      named_taxonomy = tax_list[[i]] |>
        dplyr::select(rank, name) |>
        tidyr::pivot_wider(names_from = rank, values_from = name)
      # Stitch together data frame
      tibble::tibble(taxon_id = as.character(final_id)) |>
        dplyr::bind_cols(tibble::tibble(gbif_taxonID = gbif_taxonID)) |> 
        dplyr::bind_cols(named_taxonomy)
    }
  }
  
  # Convert list to data frame
  all_taxonomies = lapply(seq_along(taxonomy_list), convert_taxonomy, taxonomy_list) |> 
    dplyr::bind_rows()
  
  # Create final data frame
  variable_order = c(query_field, "taxon_id", "gbif_taxonID", "kingdom", 
                     "phylum", "class", "order", "family", "genus", "species", 
                     "subspecies", "variety", "form")
  all_spp_taxonomies = distinct_spp |>
    dplyr::mutate(gbif_taxonID = as.character(gbif_taxonID)) |> 
    dplyr::left_join(all_taxonomies, by = "gbif_taxonID") |> 
    dplyr::select(dplyr::any_of(variable_order))
  returned_dat = dplyr::left_join(spp_list, all_spp_taxonomies, by=query_field)
  return(returned_dat)
}
