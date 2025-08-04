#' Get USGS Birds of Conservation Concern (BCC) breeding bird list for a given 
#'    Bird Conservation Region.
#' 
#' This function reads the [mpsgSE::bcc_list] data set and filters the list by a 
#'     given Bird Conservation Region.
#'
#' @param bcc_region The numeric BCC region code wrapped in quotes.
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#' install.packages(mpsgSE)
#' bcc_9 <- get_bcc_list(9)
get_bcc_list <- function(bcc_region){
  mpsgSE::bcc_list
  bcc_list[grepl(paste0("\\b", bcc_region, "\\b"), 
                 bcc_list$bc_rs_for_bcc_listing_breeding), ]
  }





#' Clip Birds of Conservation Regions to a polygon
#' 
#' This function clips the Birds of Conservaiton Regions [sf] object to a 
#'     polygon
#'
#' @param sf_lyr [sf] object.
#'
#' @returns A [tibble::tibble()].
#' @export
#'
#' @examples
#' adm_bdy <- mpsgSE::read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |> 
#'   dplyr::filter(region == "04" & forestnumber == "07")
#' get_bcc_regions(adm_bdy)
get_bcc_regions <- function(sf_lyr){
  sf::st_intersection(
    sf::st_make_valid(sf_lyr),
    sf::st_transform(mpsgSE::bcc_regions, crs = sf::st_crs(sf_lyr))
    ) |>
    sf::st_drop_geometry() |> 
    dplyr::select(-dplyr::contains("shape") & -dplyr::contains("Shape") & 
                    -dplyr::contains("SHAPE") & -dplyr::contains("gis"))
    suppressWarnings()
}