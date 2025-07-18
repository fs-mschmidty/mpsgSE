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
  bcc_list[grepl(paste0("\\b", bcc_region, "\\b"), bcc_list$bc_rs_for_bcc_listing_breeding), ]
  }
