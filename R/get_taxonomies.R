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
  distinct_x <- spp_list |>
    dplyr::distinct(eval(parse(text=query_field)), .keep_all=T) |>
    dplyr::select(query_field)

  sci_name<- distinct_x |> 
    dplyr::pull(query_field) |>
    stringr::str_replace("[\r\n]", " ") |>
    stringr::str_replace("[\r\n]", "") |>
    stringr::str_to_sentence()


  orig_id <- taxize::get_gbifid(sci_name, ask = FALSE, rows = 1, messages = FALSE)
  
  class <- taxize::classification(orig_id, db = "gbif")

  convert_taxonomy <- function(i, spp_list) {
    gbif_taxonID <- names(spp_list)[[i]]

    if (!is.na(gbif_taxonID)) {
      named_taxonomy <- spp_list[[i]] |>
        dplyr::select(rank, name) |>
        tidyr::pivot_wider(names_from = rank, values_from = name)

      final_id <- spp_list[[i]] |>
        tail(1) |> 
        dplyr::pull(id)

      named_taxonomy |>
        dplyr::bind_cols(tibble::tibble(taxon_id = as.character(final_id))) |>
        dplyr::bind_cols(tibble::tibble(gbif_taxonID = gbif_taxonID))
    }
  }
  t <- lapply(seq_along(class), convert_taxonomy, class)

  all_taxonomies <- dplyr::bind_rows(t)

  all_sp_taxonomies_table<-distinct_x |>
    dplyr::bind_cols(tibble::tibble(gbif_taxonID = as.character(orig_id))) |>
    dplyr::left_join(all_taxonomies, by = "gbif_taxonID") 

  spp_list |> dplyr::left_join(all_sp_taxonomies_table, by=query_field)

}
