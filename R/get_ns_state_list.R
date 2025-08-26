#' This function exports all species from a state with NS state rank.

#' @param state A character with state code, exmaple: "CO".
#' @param taxonomy TRUE of FALSE.  TRUE if you want to return the data with 
#'                     taxonomic information included.  This can take a long 
#'                     time for states with large lists.
#'
#' @return A list()
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


#' Get NatureServe Habitats
#' 
#' This function makes a call to the NatureServe API using the NatureServe State 
#'     List and returns all habitats associated with any given species.
#'
#' @param ns_state_list NatureServe State list from [get_ns_state_list()].
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' 
#' @return A [tibble::tibble()]
#' @export
get_ns_habitat <- function(ns_state_list, spp_list) {
  
  t_ids <- spp_list$taxon_id
  
  ns_el_data <- ns_state_list |>
    dplyr::filter(taxon_id %in% t_ids) |>
    dplyr::mutate(api_shortcode = stringr::str_extract(view_on_nature_serve_explorer, "ELEMENT_GLOBAL\\.\\d+\\.\\d+")) |>
    dplyr::mutate(ns_taxon_api_url = glue::glue("https://explorer.natureserve.org/api/data/taxon/{api_shortcode}")) |>
    dplyr::distinct() |>
    dplyr::group_by(taxon_id) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n == 1)
  
  natureserv_get_hab_data <- function(x) {
    # x = as.character(ns_el_data$ns_taxon_api_url[1])
    
    req <- httr2::request(x)
    
    resp <- req |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    
    if (resp$status_code == 200) {
      resp <- resp |>
        httr2::resp_body_json()
      
      all_hab_names <- c(
        "terrestrial",
        "marine",
        "riverine",
        "palustrine",
        "lacustrine",
        "subterrainean",
        "estuarine"
      )
      
      root_of_chars <- resp$speciesCharacteristics
      
      habitatComments <- root_of_chars$habitatComments
      
      hab_df <- tibble::tibble(
        hab_cat = c("comments"),
        ns_hab = habitatComments
      )
      
      return_habs_from_hab_cat <- function(x, h_t) {
        tibble::tibble(
          hab_cat = h_t,
          ns_hab = x[2][[1]][[2]]
        )
      }
      
      
      get_all_habs <- function(x, ls_json) {
        hab_type <- x
        hab_list <- ls_json[glue::glue("species{stringr::str_to_title(hab_type)}Habitats")]
        
        hab_list[[1]] |>
          lapply(return_habs_from_hab_cat, hab_type) |>
          dplyr::bind_rows()
      }
      
      lapply(all_hab_names, get_all_habs, root_of_chars) |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(hab_df)
    } else {
      tibble::tibble(
        hab_cat = NA,
        ns_hab = NA
      )
    }
  }
  
  get_all_ns_data <- function(t_id, ns_dat) {
    # t_id = ns_el_data$taxon_id[1]; ns_dat = ns_el_data
    sp_data <- ns_dat |>
      dplyr::filter(taxon_id == t_id)
    print(sp_data$scientific_name)
    
    natureserv_get_hab_data(as.character(sp_data$ns_taxon_api_url)) |>
      dplyr::mutate(taxon_id = t_id)
  }
  
  ns_el_data$taxon_id |> 
    lapply(get_all_ns_data, ns_el_data) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(ns_hab))
}
