#' Get a Regional Forester's Sensitive Species List
#'
#' This funciton filter's the master Regional Forester's Sensitive Species List
#'     saved as the `rfss` dataset in the `mpsgSE` package to a specified Forest
#'     Service Region.
#'
#' @param fs_region Character. Forest Service Region (e.g. "R1")
#'
#' @return [tibble::tibble()]
#' @export
#'
#' @examples
#' install.packages(mpsgSE)
#' r1_ssl <- get_rfss_list("R1")
get_rfss_list <- function(fs_region){
  ss = mpsgSE::rfss |> dplyr::filter(region == fs_region)
  return(ss)
}


#' List regions in rfss dataset
#'
#' This function lists the regions in the `rfss` dataset.
#'
#' @return A `vector()`
#' @export
#'
#' @examples
#' install.packages(mpsgSE)
#' list_regions()
list_regions <- function(){
  mpsgSE::rfss |> dplyr::pull(region) |> unique() |> sort()
}


