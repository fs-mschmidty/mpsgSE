#' This function takes in a dataframe of species with assigned taxon_ids and outputs a list of potential synonyms from the GBIF Backbone taxonomy.  If more than one species has a taxon_id assigned to it it will only returns synonyms for one species. This returns many fields, however only the cananical name is used.
#' @param x a vector of unique taxon_ids from get_taxonomies
#' @export

get_synonyms <- function(x) {
  lapply(x, function(x) rgbif::name_usage(key = x, data = "synonyms")$data) |>
    dplyr::bind_rows() |>
    dplyr::mutate(taxon_id = acceptedKey)
}
