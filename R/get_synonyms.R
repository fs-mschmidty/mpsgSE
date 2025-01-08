#' This function takes in a dataframe of species with assigned taxon_ids and outputs a list of potential synonyms from the GBIF Backbone taxonomy.  If more than one species has a taxon_id assigned to it it will only returns synonyms for one species. This returns many fields, however only the cananical name is used.
#' @param x a data frame
#' @param taxon_id_field GBIF taxon_id provided by get_taxonomies.
#' @export

get_synonyms <- function(x, taxon_id_field = "taxon_id") {
  t_ids <- x |>
    dplyr::distinct(taxon_id_field) |>
    dplyr::pull(taxon_id_field)

  lapply(t_ids, function(x) rgbif::name_usage(key = x, data = "synonyms")$data) |>
    dplyr::bind_rows() |>
    dplyr::mutate(taxon_id = acceptedKey)
}
