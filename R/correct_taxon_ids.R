#' Correct known issues with taxon ID's
#'
#' @param spp_list A data frame with taxon ID's from `get_taxonomies()`.
#'
#' @returns a data frame with corrected taxon ID's
#' @export
#'
#' @examples
#' library(mpsgSE)
#' 
#' spp_list <- mpsgSE::sp_list_ex |>
#'   get_taxonomies() |> 
#'   correct_taxon_ids()
correct_taxon_ids <- function(spp_list){
  dat = spp_list |> 
    dplyr::mutate(
      taxon_id = as.numeric(taxon_id),
      taxon_id = ifelse(scientific_name == "Anarhynchus montanus", 2480292, taxon_id),
      taxon_id = ifelse(scientific_name == "Neotamias rufus", 11062336, taxon_id),
      taxon_id = ifelse(scientific_name == "Micropterus nigricans", 5211240, taxon_id),
      taxon_id = ifelse(scientific_name == "Oncorhynchus lewisi", 5711980, taxon_id), 
      taxon_id = ifelse(scientific_name == "Furcula vargoi", 3234292, taxon_id)
      )
  return(dat)
}