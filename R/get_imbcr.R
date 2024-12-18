get_imbcr <- function(mgmt_unit, crs = NULL, file_path = NULL){
  # Define file path
  if(is.null(file_path)){
    file_path = file.path(file.path("T:/FS/NFS/PSO/MPSG/Data/ExternalData", 
                                    "2023_IMBCR_USFSdata/2023_imbcr_data.rda"))
  }
  
  # Load and filter data
  load(file_path)
  dat = dplyr::filter(imbcr_dat, MgmtUnit %in% mgmt_unit)
  
  # Re-project CRS
  if(!is.null(crs)){
    if(sf::st_crs(dat) != crs) dat = sf::st_transform(dat, crs = crs)
  }
  
  return(dat)
}


see_imbcr_mgmt_units <- function(file_path = NULL){
  if(is.null(file_path)){
    file_path = file.path(file.path("T:/FS/NFS/PSO/MPSG/Data/ExternalData", 
                                    "2023_IMBCR_USFSdata/imbcr_mgmt_units.rda"))
  }
  load(file_path)
  message("---------- IMBCR Management Units on Forest Service Land ----------")
  print(imbcr_mgmt_units)
}


imbcr_spp <- function(imbcr_data){
  locale = stringr::str_c(unique(imbcr_data$locale), collapse = ", ")
  taxa_dat = sf::st_drop_geometry(imbcr_data) |>
    dplyr::select(scientific_name, Species, BirdCode, order:species_group, 
                  locale) |> 
    dplyr::distinct(scientific_name, .keep_all = TRUE) |> 
    dplyr::rename("common_name_IMBCR" = Species, 
                  "order_eBird" = order, 
                  "family_eBird" = family, 
                  "IMBCR_locale" = locale)
  spp_stats = sf::st_drop_geometry(imbcr_data) |>
    dplyr::select(rec_id, scientific_name, Year) |> 
    dplyr::group_by(scientific_name) |> 
    dplyr::summarize(IMBCR_nObs = dplyr::n(), 
                     IMBCR_minYear = min(Year, na.rm = TRUE), 
                     IMBCR_maxYear = max(Year, na.rm = TRUE), 
                     IMBCR_recID = ifelse(IMBCR_nObs <= 6, 
                                          stringr::str_c(unique(rec_id),
                                                         collapse = ", "), 
                                          NA),
                     .groups = "drop")
  dat = dplyr::left_join(spp_stats, taxa_dat, by = "scientific_name") |> 
    dplyr::mutate(source = "IMBCR") |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name") |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}


compile_imbcr_list <- function(imbcr_unit, imbcr_buff){
  message("Processing unit species data")
  unit_list = imbcr_spp(imbcr_unit)
  message("Processing buffer species data")
  buff_list = imbcr_spp(imbcr_buff)
  message("Compiling species list")
  comp_list = rbind(add_cols(unit_list, buff_list),
                    dplyr::filter(add_cols(buff_list, unit_list), 
                                  !taxon_id %in% unit_list$taxon_id))
  return(comp_list)
}
