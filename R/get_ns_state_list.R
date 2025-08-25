#' This function exports all species from a state with NS state rank.

#' @param state A character with state code, exmaple: "CO".
#' @param taxonomy TRUE of FALSE.  TRUE if you want to return the data with 
#'                     taxonomic information included.  This can take a long 
#'                     time for states with large lists.
#'
#' @export
get_ns_state_list <- function(state, taxonomy = TRUE) {
  get_state_rank <- function(x, state_code) {
    regex <- paste0(state_code, " \\(([^)]+)\\)")

    x |>
      stringr::str_split_1("\\\n") |>
      stringr::str_subset("^United States") |>
      stringr::str_extract(regex) |>
      stringr::str_extract("\\(([^)]+)\\)") |>
      stringr::str_replace_all("\\(|\\)", "")
  }

  export <- natserv::ns_export(location = list(nation = "US", subnation = state), 
                               format = "xlsx")
  res <- natserv::ns_export_status(export)

  while (res$state != "Finished") {
    res <- natserv::ns_export_status(export)
  }

  httr2::request(res$data$url) |>
    httr2::req_perform(tmpf <- tempfile(fileext = ".xlsx"))

  sss <- readxl::read_excel(tmpf, skip = 1) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(nature_serve_global_rank)) |>
    dplyr::mutate(
      source = glue::glue("{state} Natureserve Export")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate("{state}_sRank" := get_state_rank(distribution, state)) |>
    dplyr::ungroup()

  if (taxonomy) {
    sss <- sss |>
      mpsgSE::get_taxonomies()
  }

  l <- list()
  l[[glue::glue("{state}_export_id")]] <- export
  l[[glue::glue("{state}_natureserve_api_response")]] <- res
  l[[glue::glue("{state}_natureserve_state_list")]] <- sss
  l
}
