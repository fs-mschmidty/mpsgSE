#' Download eBird range maps 
#'
#' This function downloads range maps from eBird using the `ebirdst` package.
#'     
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' @param output_path Directory path for range maps.
#' @param ebird_access_key eBird access key. Get a key by filling out the 
#'                             request form at <https://ebird.org/st/request>.
#'
#' @return A [tibble::tibble()]
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_taxonomies()], [download_ebird_status_maps()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' ebird_status <- download_ebird_status_maps(spp_list, 
#'                                            output_path = file.path("data/ebirdst"), 
#'                                            ebird_access_key = ebird_key)
#' ebird_maps <- download_ebird_range_maps(spp_list,
#'                                         output_path = file.path("data/ebirdst"),
#'                                         ebird_access_key = ebird_key)
#' 
#' ## End(Not run)                     
download_ebird_range_maps = function(spp_list, output_path, ebird_access_key){

  # evaluate output directory
  if(!dir.exists(output_path)) dir.create(output_path)
  
  eligible_birds = spp_list |>
    dplyr::filter(class == "Aves") |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::left_join(ebirdst::ebirdst_runs, by = "scientific_name") |> 
    dplyr::filter(has_trends == TRUE)
  
  get_bird_map = function(t_id) {
    bird_data = dplyr::filter(eligible_birds, taxon_id == t_id)
    sci_name = bird_data$scientific_name
    
    ebirdst::load_ranges(sci_name, resolution = "27k", path = output_path) |>
      dplyr::mutate(
        season = dplyr::case_when(season == "breeding" ~ "Breeding",
                                  season == "nonbreeding" ~ "Nonbreeding",
                                  TRUE ~ "Migration"),
        order = dplyr::case_when(season == "Breeding" ~ 1,
                                 season == "Nonbreeding" ~ 2,
                                 TRUE ~ 3),
        scientific_name = sci_name,
        taxon_id = t_id
      ) |>
      dplyr::arrange(desc(order))
  }
  
  range_maps = lapply(eligible_birds$taxon_id, get_bird_map) |> 
    dplyr::bind_rows()
  return(range_maps)
}


#' Download eBird status maps
#' 
#' This function downloads status maps from eBird using the `ebirdst` package.
#'
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' @param output_path Directory path for status maps.
#' @param ebird_access_key eBird access key. Get a key by filling out the 
#'                             request form at <https://ebird.org/st/request>.
#'
#' @return [tibble::lst()]
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_taxonomies()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' ebird_status <- download_ebird_status_maps(spp_list, 
#'                                            output_path = file.path("data/ebirdst"), 
#'                                            ebird_access_key = ebird_key)
#' 
#' ## End(Not run)                     
download_ebird_status_maps = function(spp_list, output_path, ebird_access_key){
  
  ebirdst::set_ebirdst_access_key(ebird_access_key, overwrite = TRUE)
  # evaluate output directory
  if(!dir.exists(output_path)) dir.create(output_path)
  
  # eBird status & trend information
  eligible_birds = spp_list |>
    dplyr::filter(class == "Aves") |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::left_join(ebirdst::ebirdst_runs, by = "scientific_name")

  birds_w_status_maps = dplyr::filter(eligible_birds, has_trends == TRUE)
  birds_wo_status_maps = dplyr::filter(eligible_birds, has_trends != TRUE)
  eligible_birds_sci_names = dplyr::pull(birds_w_status_maps, scientific_name)
  
  lapply(eligible_birds_sci_names, 
         ebirdst::ebirdst_download_status, download_ranges = T, 
         pattern = "range_smooth_27km|range_smooth_9km", path = output_path)

  tibble::lst(birds_w_status_maps, birds_wo_status_maps)
}


#' Download eBird trend maps
#' 
#' This function downloads trend maps from eBird using the `ebirdst` package.
#'
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' @param output_path Directory path for trend data.
#' @param ebird_access_key eBird access key. Get a key by filling out the 
#'                             request form at <https://ebird.org/st/request>.
#' 
#' @return A [tibble::tibble()]
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_taxonomies()], [download_ebird_status_maps()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' ebird_status <- download_ebird_status_maps(spp_list, 
#'                                            output_path = file.path("data/ebirdst"), 
#'                                            ebird_access_key = ebird_key)
#' ebird_maps <- download_ebird_trends_maps(spp_list,
#'                                          output_path = file.path("data/ebirdst"),
#'                                          ebird_access_key = ebird_key)
#' 
#' ## End(Not run)                     
download_ebird_trends_maps <- function(spp_list, output_path, ebird_access_key){
  
  ebirdst::set_ebirdst_access_key(ebird_access_key, overwrite = TRUE)
  # evaluate output directory
  if(!dir.exists(output_path)) dir.create(output_path)
  
  eligible_birds = spp_list |>
    dplyr::filter(class == "Aves") |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::left_join(ebirdst::ebirdst_runs, by = "scientific_name") |> 
    dplyr::filter(has_trends == TRUE)
  
  lapply(eligible_birds$scientific_name, ebirdst::ebirdst_download_trends, 
         path = output_path)
  bird_trend_maps = lapply(eligible_birds$scientific_name, ebirdst::load_trends, 
                       path = output_path) |> 
    dplyr::bind_rows() |> 
    dplyr::left_join(dplyr::select(eligible_birds, taxon_id, species_code), 
                     by = "species_code")
  return(bird_trend_maps)
  }


#' Download regional trend statistics from eBird
#'
#' This function downloads regional trend statistics from eBird using the 
#'     `ebirdst` package.
#' 
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' @param ebird_access_key eBird access key. Get a key by filling out the 
#'                             request form at <https://ebird.org/st/request>.
#'
#' @return A [tibble::tibble()]
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_taxonomies()], [download_ebird_status_maps()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' ebird_reg_status <- get_ebird_regional_stats(spp_list, ebird_access_key = ebird_key)
#' 
#' ## End(Not run)                     
get_ebird_regional_stats <- function(spp_list, ebird_access_key){

  ebird_spp <- spp_list |>
    dplyr::filter(class == "Aves") |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::left_join(ebirdst::ebirdst_runs, by = "scientific_name") |> 
    dplyr::filter(has_trends == TRUE)
  
  regional_stats <- lapply(1:nrow(ebird_spp), function(sp){
    t_id <- ebird_spp$taxon_id[sp]
    sp_code <- ebird_spp$species_code[sp]
    
    url <- glue::glue("https://st-download.ebird.org/v1/list-obj/2023/{sp_code}?key={ebird_access_key}")
    req <- httr2::request(url)
    resp <- req |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (resp$status_code == 200) {
      resp <- httr2::resp_body_json(resp)
      api_dat <- unlist(resp)
      csv_dat <- api_dat[grepl("\\.csv$", api_dat)]
      
      rs_url <- glue::glue("https://st-download.ebird.org/v1/fetch?objKey={csv_dat[2]}&key={ebird_access_key}")
      rs_dat <- readr::read_csv(rs_url, show_col_types = FALSE) |> 
        dplyr::mutate(taxon_id = t_id)
    }
    
    return(rs_dat)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::filter(stringr::str_detect(continent_name, "USA"))
  
  return(regional_stats)
}


#' Calculate abundance trends for bird species
#' 
#' This function calculates trends in bird species abundance from eBird data 
#'     using the `ebirdst` package. 
#'
#' @param spp_list Species list with taxonomy from [get_taxonomies()].
#' @param output_path Directory path for trend data.
#' @param ebird_access_key eBird access key. Get a key by filling out the 
#'                             request form at <https://ebird.org/st/request>.
#'
#' @return A [tibble::tibble()]
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_taxonomies()], [download_ebird_status_maps()]
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library(mpsgSE)
#' spp_list <- get_taxonomies(sp_list_ex)
#' ebird_status <- download_ebird_status_maps(spp_list, 
#'                                            output_path = file.path("data/ebirdst"), 
#'                                            ebird_access_key = ebird_key)
#' ebird_trends <- get_ebird_trends(spp_list,
#'                                  output_path = file.path("data/ebirdst"),
#'                                  ebird_access_key = ebird_key)
#' 
#' ## End(Not run)                     
get_ebird_trends <- function(spp_list, output_path, ebird_access_key){
  
  ebirdst::set_ebirdst_access_key(ebird_access_key, overwrite = TRUE)
  # evaluate output directory
  if(!dir.exists(output_path)) dir.create(output_path)
  
  eligible_birds = spp_list |>
    dplyr::filter(class == "Aves") |>
    dplyr::select(taxon_id, scientific_name) |>
    dplyr::left_join(ebirdst::ebirdst_runs, by = "scientific_name") |> 
    dplyr::filter(has_trends == TRUE) |> 
    dplyr::mutate(n_yrs = trends_end_year - trends_start_year)
  
  # Pull trend data from eBird
  lapply(eligible_birds$scientific_name, ebirdst::ebirdst_download_trends, 
         path = output_path)
  
  # Calculate percent per year trends
  ppy_trends = lapply(eligible_birds$species_code, 
                           ebirdst::load_trends, fold_estimates = TRUE,
                           path = output_path) |> 
    dplyr::bind_rows() |> 
    dplyr::group_by(species_code, fold) |> 
    # estimate range-wide trend for each fold
    dplyr::summarize(abd_ppy = sum(abd_ppy * abd) / sum(abd)) |>
    # median and confidence limits
    dplyr:: summarise(abd_ppy_median = median(abd_ppy, na.rm = TRUE),
                      abd_ppy_lower = quantile(abd_ppy, 0.10, na.rm = TRUE),
                      abd_ppy_upper = quantile(abd_ppy, 0.90, na.rm = TRUE),
                      .groups = "drop")
  # Calculate abundance trends
  abd_trends = lapply(eligible_birds$species_code, function(sp_code){
    # sp_code = eligible_birds$species_code[1]
    eb = eligible_birds[eligible_birds$species_code == sp_code, ]
    abd = ppy_trends[ppy_trends$species_code == sp_code, ] |> 
      dplyr::mutate(
        abd_trend_median = ebirdst::convert_ppy_to_cumulative(abd_ppy_median, 
                                                              eb$n_yrs),
        abd_trend_lower = ebirdst::convert_ppy_to_cumulative(abd_ppy_lower, 
                                                             eb$n_yrs),
        abd_trend_upper = ebirdst::convert_ppy_to_cumulative(abd_ppy_upper, 
                                                             eb$n_yrs)
        ) |> 
      dplyr::select(species_code, abd_trend_median, abd_trend_lower, 
                    abd_trend_upper)
    return(abd)
    }) |>
    dplyr::bind_rows()
  # Combine trends
  bird_trends = dplyr::left_join(ppy_trends, abd_trends, by = "species_code") |>
    # convert to cumulative trends
    dplyr::left_join(dplyr::select(eligible_birds, species_code, n_yrs, 
                                   taxon_id), 
                     by = "species_code")
  return(bird_trends)
}









