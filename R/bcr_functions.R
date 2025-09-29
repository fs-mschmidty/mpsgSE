#' Get USGS Birds of Conservation Concern (BCC) breeding bird list for National 
#'     Forest or Grassland.
#' 
#' This function returns a data frame of bird species that occur in the Birds of 
#'     Conservation Regions a National Forest or Grassland overlaps.
#'
#' @param sf_lyr An `sf` object of the Administrative Boundary of a National 
#'                   Forest or Grassland.
#'
#' @returns A [tibble::tibble()].
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Pull the Administrative Boundary for the Dixie National Forest
#' adm_bdy = mpsgSE::read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |>
#'   dplyr::filter(region == "04" & forestnumber == "07")
#' 
#' # Get list of birds of conservation concern
#' mpsgSE::get_bcr_list(adm_bdy)
#'  
#' ## End(Not run)                     
get_bcr_list <- function(sf_lyr){
  bcrs = mpsgSE::get_bcc_regions(sf_lyr) |> dplyr::pull(bcr_label)
  bcc = mpsgSE::bcc_list
  
  get_list = function(bcr_code){
    bcc[grepl(paste0("\\b", bcr_code, "\\b"), bcc$bc_rs_for_bcc_listing_breeding), ]
    }
  
  bcr_list = lapply(bcrs, get_list) |> 
    dplyr::bind_rows() |> 
    dplyr::distinct() |> 
    tibble::tibble()
  return(bcr_list)
  }


#' Clip Birds of Conservation Regions to a polygon
#' 
#' This function clips the Birds of Conservation Regions [sf] object to a 
#'     polygon.
#'
#' @param sf_lyr [sf] polygon object.
#'
#' @returns A [tibble::tibble()].
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Pull the Administrative Boundary for the Dixie National Forest
#' adm_bdy <- mpsgSE::read_edw_lyr("EDW_ForestSystemBoundaries_01", layer = 1) |> 
#'   dplyr::filter(region == "04" & forestnumber == "07")
#' get_bc_regions(adm_bdy)
#'  
#' ## End(Not run)                     
get_bc_regions <- function(sf_lyr){
  sf::st_intersection(
    sf::st_make_valid(sf_lyr),
    sf::st_transform(mpsgSE::bcc_regions, crs = sf::st_crs(sf_lyr))
    ) |>
    sf::st_drop_geometry() |> 
    dplyr::select(-dplyr::contains("shape") & -dplyr::contains("Shape") & 
                    -dplyr::contains("SHAPE") & -dplyr::contains("gis")) |> 
    tibble::tibble() |> 
    suppressWarnings()
}
