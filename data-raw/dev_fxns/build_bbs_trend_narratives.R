#' Build Narratives for Breeding Bird Survey Trend Data
#' 
#' This function builds the USGS Breeding Bird Survey (BBS) trend narratives for 
#'     bird species. The USGS breeding bird survey data can be found here:
#'     https://www.sciencebase.gov/catalog/item/67aba702d34e329fb20457b1
#'
#' @param region_codes Vector of BBS region codes.
#' @param region_names Vector of BBS region names.
#' 
#' @returns A [tibble::tibble()].
#' 
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' bbs_trends <-  build_bbs_trend_narratives(
#'   region_codes = c("IDA", "MON", "S10", "US1"),
#'   region_names = c("Idaho", "Montana",
#'                    "Northern Rockies Bird Conservation Region (S10)",
#'                    "the United States")
#'   )
#'  
#' ## End(Not run)                     
build_bbs_trend_narratives <- function(region_codes, region_names){
  region_df = tibble::tibble(region = region_codes,
                             region_long_name = region_names)
  
  bbs_narratives = mpsgSEdata::core_trend |>
    dplyr::filter(region %in% region_df$region) |>
    dplyr::left_join(region_df, by = "region") |>
    dplyr::mutate(
      trend_description = dplyr::case_when(significance == 1 ~ "increasing",
                                           significance == 2 ~ "decreasing",
                                           is.na(significance) ~ "uncertain"),
      narrative_chunk = glue::glue(
        "in {region_long_name} the trend is {trend_description} with a trend of {trend}% [95% CI {x2_5_percent_ci}%, {x97_5_percent_ci}%]"
      )
    ) |> 
    dplyr::group_by(species) |>
    dplyr::summarize(
      narrative_total_chunk = paste(narrative_chunk, collapse = "; "),
      n = dplyr::n()
    ) |>
    dplyr::mutate(
      citation = "(Hostelter et al. 2025 via USDA Forest Service 2025)",
      narrative_total_chunk = ifelse(
        n == 2, 
        stringr::str_replace(narrative_total_chunk, ";", " and"),
        stringr::str_replace(narrative_total_chunk, ";(?=[^;]*$)", ", and")
      ),
      narrative_total_chunk = stringr::str_replace(narrative_total_chunk, 
                                                   "i", "I"),
      narrative_total_chunk = stringr::str_replace_all(narrative_total_chunk, 
                                                       ";", ","),
      final_narrative = glue::glue(
        "The USGS estimates population trend with data from the North American Breeding Bird Survey collected from 1966 to 2022. {narrative_total_chunk} {citation}."
      )
    )
  
  return(bbs_narratives)
}
