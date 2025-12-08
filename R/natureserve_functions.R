#' Build NatureServe Species List
#'
#' This function builds a species list with global and state NatureServe
#'     rankings using data returned from `get_ns_state_list()`.
#'
#' @param ns_data NatureServe data from `get_ns_state_list()`.
#'
#' @return A [tibble::tibble()]
#'
#' @seealso [get_ns_state_list()]
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#' ns_co <- get_ns_state_list("CO", taxonomy = FALSE)
#' spp_list <- build_ns_spp_list(ns_co)
#'
#' ## End(Not run)
build_ns_spp_list <- function(ns_data){
  # ns_data = ns_co
  
  # columns to select
  col_select1 <- c("taxon_id", "gbif_taxonID", "element_code",
                   "scientific_name", "common_name",
                   "species_group_broad", "species_group_fine",
                   "nature_serve_global_rank", 
                   "nature_serve_rounded_global_rank",
                   "u_s_endangered_species_act_status", "sara_status")
  col_select2 = c("distribution", "view_on_nature_serve_explorer",
                  "kingdom", "phylum", "class", "order", "family", "genus",
                  "species", "subspecies", "variety", "form", "synonyms",
                  "source")
  
  
  dat = ns_data[
    names(ns_data)[stringr::str_detect(names(ns_data), "state_list")]
    ][[1]] |>
    dplyr::select(dplyr::any_of(col_select1), dplyr::contains("sRank"),
                  dplyr::any_of(col_select2)) |>
    dplyr::distinct() |>
    dplyr::rename('broad_group' = species_group_broad,
                  'fine_group' = species_group_fine,
                  'esa_status' = u_s_endangered_species_act_status,
                  'gRank' = nature_serve_global_rank,
                  'rounded_gRank' = nature_serve_rounded_global_rank)
  return(dat)
}


#' Combine Two NatureServe Habitat Data Frames
#'
#' This function combines two habitat lists returned from `get_ns_habitat()`.
#'
#' @param state_a_habitats Idaho NatureServe Habitats from this pipeline.
#' @param state_b_habitats Montana NatureServe Habitats from this pipeline
#'
#' @returns A [tibble::tibble()]
#'
#' @seealso [get_ns_habitat()]
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#'
#' # Colorado
#' ns_co <- get_ns_state_list("CO")
#' hab_co <- get_ns_habitat(ns_co)
#'
#' # Wyoming
#' ns_wy <- get_ns_state_list("WY")
#' hab_wy <- get_ns_habitat(ns_wy)
#'
#' # Combine CO and WY
#' habitats <- combine_ns_habs(hab_co, hab_wy)
#'
#' ## End(Not run)
combine_ns_habs <- function(state_a_habitats, state_b_habitats){
  ns_habitats = dplyr::full_join(state_a_habitats, state_b_habitats,
                             by = c("habitat_category", "ns_habitat_type"),
                             relationship = 'many-to-many') |>
    dplyr::mutate(taxon_id = ifelse(taxon_id.x == taxon_id.y,
                                    taxon_id.x, c(taxon_id.x, taxon_id.y))) |>
    dplyr::select(-taxon_id.x, -taxon_id.y) |>
    dplyr::distinct()
  return(ns_habitats)
}


#' Combine NatureServe Data
#' 
#' This function combines the state NatureServe lists from the 
#'     `get_ns_state_list()` function in this pipeline.
#'
#' @param ns_list_1 State data from `get_ns_state_list()`.
#' @param ns_list_2 State data from `get_ns_state_list()`.
#'
#' @return A [tibble::tibble()]
#'
#' @seealso [get_ns_state_list()]
#'
#' @export
#'
#' @examples
#' library("mpsgSE")
#'
#' ns_co <- get_ns_state_list("CO")
#' ns_ks <- get_ns_state_list("KS")
#' ns_data <- combine_natureserve_data(ns_co, ns_ks)
combine_natureserve_data <- function(ns_list_1, ns_list_2){
  # Function to pull species list from NatureServe List
  pull_spp_list = function(ns_list){
    dat = ns_list[
      names(ns_list)[stringr::str_detect(names(ns_list), "state_list")]
    ][[1]]
    my_list = list(
      base_info = dplyr::select(dat, taxon_id, gbif_taxonID, element_code,
                                scientific_name, common_name, 
                                species_group_broad, species_group_fine,
                                nature_serve_global_rank:sara_status,
                                distribution:view_on_nature_serve_explorer, 
                                kingdom:species, subspecies:form, synonyms) |>
        dplyr::distinct(),
      state_rank = dplyr::select(dat, taxon_id, dplyr::contains("sRank")) |>
        dplyr::distinct(), 
      source = dplyr::select(dat, taxon_id, source) |> dplyr::distinct()
    )
    return(my_list)
  }
  
  # Process State NatureServe Lists
  sl1 = pull_spp_list(ns_list_1)
  sl2 = pull_spp_list(ns_list_2)
  base_info = dplyr::bind_rows(sl1$base_info, sl2$base_info) |> 
    dplyr::distinct()
  state_rank = dplyr::bind_rows(sl1$state_rank, sl2$state_rank) |>
    dplyr::distinct()
  ns_source = dplyr::bind_rows(sl1$source, sl2$source) |> 
    dplyr::group_by(taxon_id) |> 
    dplyr::summarise(source = stringr::str_c(source, collapse = "; ")) |> 
    dplyr::distinct()
  
  
  # Combine lists into one dataframe
  ns_dat = dplyr::left_join(base_info, state_rank, by = "taxon_id",
                            relationship = 'many-to-many') |>
    # dplyr::left_join(ns_source, by = "taxon_id", 
    #                  relationship = 'many-to-many') |> 
    dplyr::distinct()
  return(ns_dat)
}


#' Count Species by NatureServe Habitat Type
#'
#' This function creates a data frame that counts the number of species by
#'     habitat in a data frame returned from `get_ns_habitat()`.
#'
#' @param ns_habitats Data frame from `get_ns_habitat()`.
#'
#' @return A [tibble::tibble()].
#'
#' @seealso [get_ns_habitat()]
#'
#' @export
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#' ns_co <- get_ns_state_list("CO")
#' habitats <- get_ns_habitat(ns_co)
#' habitat_xwalk <- count_spp_by_hab(habitats)
#'
#' ## End(Not run)
count_spp_by_hab <- function(ns_habitats){
  df = ns_habitats |>
    dplyr::filter(!habitat_category == "comments") |>
    dplyr::ungroup() |>
    dplyr::summarise(n_spp = dplyr::n(),
                     .by = c("habitat_category", "ns_habitat_type")) |>
    dplyr::mutate(mpsg_habitat = NA) |>
    dplyr::arrange(habitat_category, ns_habitat_type)
  return(df)
}


#' Import NatureServe Species Data
#'
#' This function imports NatureServe species data for a given state into R.
#'
#' @param state A character with state code, example: "CO".
#' @param taxonomy TRUE of FALSE.  TRUE if you want to return the data with
#'                     taxonomic information included.  This can take a long
#'                     time for states with large lists.
#'
#' @return A [list()]
#' @export
#'
#' @examples
#' library("mpsgSE")
#' co_ns_data_no_taxonid <- get_ns_state_list("CO", taxonomy = FALSE)
#' co_ns_data <- get_ns_state_list("CO")
get_ns_state_list <- function(state, taxonomy = TRUE) {
  # state = "CO"; taxonomy = TRUE

  # Function to pull S-ranks
  get_state_rank <- function(x, state_code) {
    regex <- paste0(state_code, " \\(([^)]+)\\)")

    x |>
      stringr::str_split_1("\\\n") |>
      stringr::str_subset("^United States") |>
      stringr::str_extract(regex) |>
      stringr::str_extract("\\(([^)]+)\\)") |>
      stringr::str_replace_all("\\(|\\)", "")
  }
  
  # Generate request from API
  export <- natserv::ns_export(location = list(nation = "US", subnation = state),
                               format = "xlsx")
  res <- natserv::ns_export_status(export)
  while (res$state != "Finished") res <- natserv::ns_export_status(export)
  # execute API request and save as temp xlsx file
  httr2::request(res$data$url) |>
    httr2::req_perform(tmpf <- tempfile(fileext = ".xlsx"))
  
  # pull species list from temp xlsx
  sss <- readxl::read_excel(tmpf, skip = 1) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(nature_serve_global_rank)) |>
    dplyr::mutate(
      source = glue::glue("{state} Natureserve Export")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate("{state}_sRank" := get_state_rank(distribution, state)) |>
    dplyr::ungroup() |> 
    dplyr::distinct()
  # get taxon ids and taxonomies
  if (taxonomy) sss <- mpsgSE::get_taxonomies(sss)
  
  # construct output list
  l <- list()
  l[[glue::glue("{state}_export_id")]] <- export
  l[[glue::glue("{state}_natureserve_api_response")]] <- res
  l[[glue::glue("{state}_natureserve_state_list")]] <- sss
  
  return(l)
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
#'
#' @examples
#' ## Not run:
#'
#' library("mpsgSE")
#' ns_co <- get_ns_state_list("CO")
#' habitats <- get_ns_habitat(ns_co)
#'
#' ## End(Not run)
get_ns_habitat <- function(ns_state_list, spp_list) {
  # ns_state_list = targets::tar_read(ns_list)
  # spp_list = targets::tar_read(elig_list)

  t_ids = spp_list$taxon_id
  
  dat = if(tibble::is_tibble(ns_state_list)){
    ns_state_list
    } else if(is.list(ns_state_list)){
      dat = ns_state_list[names(ns_state_list)[
        stringr::str_detect(names(ns_state_list), "natureserve_state_list")
        ]][[1]]
      } else (message("Data type not recognized. Check 'ns_state_list'."))
  
  ns_el_data = ns_state_list |>
    dplyr::filter(taxon_id %in% t_ids) |>
    dplyr::mutate(
      api_shortcode = stringr::str_extract(view_on_nature_serve_explorer,
                                           "ELEMENT_GLOBAL\\.\\d+\\.\\d+"),
      ns_taxon_api_url = glue::glue("https://explorer.natureserve.org/api/data/taxon/{api_shortcode}")) |>
    dplyr::distinct() |>
    dplyr::group_by(taxon_id) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n == 1)

  natureserv_get_hab_data = function(x) {
    # x = as.character(ns_el_data$ns_taxon_api_url[1])

    req = httr2::request(x)
    resp = req |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()


    #-- Explore resp
    # lapply(1:length(resp), function(l){
    #   message("Names:")
    #   print(names(resp)[l])
    #   message("Data type:")
    #   print(class(resp[[l]]))
    #   message("Data names:")
    #   print(names(resp[[l]]))
    #   # if(length(resp[[l]]) < 20){
    #   #   message("Data:")
    #   #   print(resp[[l]])
    #   #   }
    #   readline(prompt = "Press Enter to continue...")
    # })
    #
    # # Range Extent
    # resp$rankInfo$rangeExtentComments
    # # Threat Impacts
    # resp$rankInfo$threatImpactComments
    # # Trends
    # resp$rankInfo$shortTermTrendComments
    # resp$rankInfo$longTermTrendComments
    # # Habitat Comments
    # resp$speciesCharacteristics$habitatComments


    if (resp$status_code == 200) {
      resp = httr2::resp_body_json(resp)
      all_hab_names = c("terrestrial", "marine", "riverine", "palustrine",
                         "lacustrine", "subterrainean", "estuarine")
      root_of_chars = resp$speciesCharacteristics
      hab_df = tibble::tibble(habitat_category = "comments",
                               ns_habitat_type = root_of_chars$habitatComments)

      return_habs_from_hab_cat = function(x, h_t) {
        tibble::tibble(habitat_category = h_t,
                       ns_habitat_type = x[2][[1]][[2]])
        }

      get_all_habs = function(x, ls_json) {
        hab_type = x
        hab_list = ls_json[glue::glue("species{stringr::str_to_title(hab_type)}Habitats")]
        hab_list[[1]] |>
          lapply(return_habs_from_hab_cat, hab_type) |>
          dplyr::bind_rows()
        }

      lapply(all_hab_names, get_all_habs, root_of_chars) |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(hab_df)
      } else {
        tibble::tibble(habitat_category = NA, ns_habitat_type = NA)
      }
    } # End of natureserv_get_hab_data()

  get_all_ns_data = function(t_id, ns_dat) {
    # t_id = ns_el_data$taxon_id[1]; ns_dat = ns_el_data
    sp_data = ns_dat |>
      dplyr::filter(taxon_id == t_id)
    print(sp_data$scientific_name)

    natureserv_get_hab_data(as.character(sp_data$ns_taxon_api_url)) |>
      dplyr::mutate(taxon_id = t_id)
  }

  hab_dat = ns_el_data$taxon_id |>
    lapply(get_all_ns_data, ns_el_data) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(ns_habitat_type))

  return(hab_dat)
}


#' DEPRECATED: Generate a species list with NatureServe G- and S-ranks
#'
#' This function is deprecated. Use `get_ns_state_list()` and
#'     `build_ns_spp_list()` instead.
#'
#' This function uses the `natserv` package to query the NatureServe API
#'     and read those data into R. This function then converts the JSON
#'     data returned into a `tibble::tibble()`.
#
#' @param species_list A character vector of species names.
#' @param states A character vector of 2-letter US state codes. Default is NULL, if
#'     NULL all US states are returned.
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' spp = c("Buteo jamaicensis", "Falco peregrinus", "Aquila chrysaetos")
#' ns_ranks(spp, c("UT", "CO"))
ns_ranks <- function(species_list, states = NULL) {
  #-- Clean species list
  spp <- species_list[!is.na(species_list)] |>
    unique() |>
    trimws()
  #-- Assign states
  if (is.null(states)) states <- state.abb

  #-- Notify user of time to pull data
  message(
    paste0(
      length(spp), " unique species. Approximate time to complete: ",
      round((length(spp) * 0.557) / 60, 0), " minuntes"
    )
  )
  #-- Query NatureServe API for each species in the list
  dat <- lapply(spp, function(sp) {
    # sp = spp[3]
    #-- Courtesy count
    message(paste(match(sp, spp), " "), appendLF = FALSE)
    #-- Pull NatureServe data
    nsdat <- try(natserv::ns_search_spp(text = sp))
    #-- Validate data and munge if data are good
    if (nrow(nsdat$results) > 0) {
      #-- Find and validate indices
      # Index of input species name
      i <- which(sapply(nsdat$results$scientificName, FUN = function(x) sp %in% x))
      # Validate i
      if (length(i) > 1) i <- min(i)
      if (length(i) == 0) i <- 1
      #-- Create initial data
      dat <- tibble::tibble(dplyr::select(nsdat$results, -speciesGlobal)[i, ]) |>
        dplyr::bind_cols(nsdat$results$speciesGlobal[i, ]) |>
        dplyr::select(
          scientificName, primaryCommonName, roundedGRank, gRank,
          usesaCode, kingdom:genus
        ) |>
        dplyr::rename(
          "scientific_name" = scientificName,
          "common_name" = primaryCommonName,
          "rounded_gRank" = roundedGRank,
          "usfws_esa" = usesaCode, "class" = taxclass,
          "order" = taxorder
        ) |>
        dplyr::mutate(input_name = sp) |>
        dplyr::mutate_if(is.logical, as.character)
      #-- Find and validate indices
      # Validate S-Rank data
      if (length(nsdat$results$nations[[i]]) == 0) {
        return()
      }
      # Index of s-rank and nativity list
      y <- try(which(sapply(nsdat$results$nations[[i]]$nationCode,
                            FUN = function(x) "US" %in% x
      )))
      # Validate y
      if (length(y) == 0) {
        return()
      }
      # Validate i & y
      if (length(nsdat$results$nations[[i]]$subnations[[y]]) == 0) {
        return()
      }
      #-- Pull S-ranks
      sranks <- nsdat$results$nations[[i]]$subnations[[y]] |>
        dplyr::filter(subnationCode %in% states) |>
        dplyr::mutate(state_srank = paste0(subnationCode, "_sRank")) |>
        dplyr::select(state_srank, roundedSRank) |>
        dplyr::arrange(state_srank) |>
        tidyr::pivot_wider(
          names_from = state_srank, values_from = roundedSRank,
          values_fill = NA
        )
      #-- Pull nativity
      nativity <- nsdat$results$nations[[i]]$subnations[[y]] |>
        dplyr::filter(subnationCode %in% states) |>
        dplyr::mutate(state_nat = paste0(subnationCode, "_native_NS")) |>
        dplyr::select(state_nat, native) |>
        dplyr::arrange(state_nat) |>
        tidyr::pivot_wider(
          names_from = state_nat, values_from = native,
          values_fill = FALSE
        )
      #-- Create final data frame
      dat <- dat |>
        dplyr::bind_cols(sranks) |>
        dplyr::bind_cols(nativity)
    }
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(
      scientific_name:gRank, dplyr::contains("srank"), usfws_esa,
      dplyr::contains("native"), kingdom:genus, input_name
    ) |>
    dplyr::arrange(kingdom, phylum, class, order, family, genus)
  return(dat)
  message("", appendLF = FALSE)
}

