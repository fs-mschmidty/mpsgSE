

#' Read IMBCR trend data into R
#' 
#' This function read an *.RDS file into R and filters it by Birds of 
#'     Conservation Consern (BCC) region. 
#'
#' @param bcc_region Character vector of BCC regions. 
#'
#' @returns A [tibble::tibble()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' regions <- c("UT-BCR9", "UT-BCR16", "UT")
#' imbcr_trends <- get_imbcr_trends(regions)
#' 
#' ## End(Not run)                     
get_imbcr_trends <- function(bcc_region){
  
  imbcr_trends = readRDS(file = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted", 
                                          "Species/IMBCR_USFSdata", 
                                          "imbcr_trends.RDS")) |> 
    dplyr::filter(stratum %in% bcc_region)
  return(imbcr_trends)
  
}

