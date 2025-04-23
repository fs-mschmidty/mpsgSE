#' Read IMBCR Data into R.
#' 
#' This function reads IMBCR data into R. The data are stored in an *.RDS file
#'    on the T-drive. The user can specify a directory path if using a copy of 
#'    the *.rda file on your local machine.
#'
#' @param mgmt_unit The IMBCR management unit. Use `see_imbcr_mgmt_units()` to 
#'                      see management unit names.
#' @param crs Target coordinate reference system (CRS). Either and 
#'                [sf::st_crs()] object or accepted input string (e.g. "NAD83"). 
#'                See [sf::st_crs()] for more details. Default is NULL. If NULL, 
#'                resulting sf object CRS will be NAD83.
#' @param dir_path Path to the IMBCR data folder. Default is NULL. If NULL, data 
#'                     will be read from the MPSG T-drive folder.
#'
#' @return An [sf] class object.
#' 
#' @details
#' You must be connected to the T-drive, e.g., through VPN, to use this function
#'     unless you have the *.rda file on your local machine. 
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' mgmt_units <- c("Cimarron National Grassland", "Comanche National Grassland")
#' get_imbcr(mgmt_units)
#' 
#' ## End(Not run)                     
get_imbcr <- function(mgmt_unit, crs = NULL, dir_path = NULL){
  # Define file path
  if(is.null(dir_path)){
    dir_path = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species", 
                         "2023_IMBCR_USFSdata")
  }

  # IMBCR Data ----
  message("Reading IMBCR data")
  dat = readRDS(file.path(dir_path, "imbcr_data.RDS")) |>
    dplyr::filter(MgmtUnit %in% mgmt_unit) |> 
    dplyr::filter(!stringr::str_detect(Species, "Unknown"))
  
  # Re-project CRS
  if(!is.null(crs)){
    if(sf::st_crs(dat) != crs) dat = sf::st_transform(dat, crs = crs)
  }
  
  # Return data
  return(dat)
}


#' View IMBCR Management Units
#'
#' This function reads IMBCR data into R. The data are stored in an *.rda file
#'    on the T-drive. The user can specify a file path if using a copy of the 
#'    *.rda file on your local machine.
#'    
#' @param file_path Path to *.rda file.
#'
#' @return A vector.
#' 
#' @details
#' You must be connected to the T-drive, e.g., through VPN, to use this function
#'     unless you have the *.rda file on your local machine. 
#' 
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' see_imbcr_mgmt_units()
#' 
#' ## End(Not run)                     
see_imbcr_mgmt_units <- function(file_path = NULL){
  if(is.null(file_path)){
    rda_path = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species",
                         "2023_IMBCR_USFSdata/imbcr_mgmt_units.RDS")
  }
  readRDS(rda_path)
  message("---------- IMBCR Management Units on Forest Service Land ----------")
  print(imbcr_mgmt_units)
}


#' Summarize IMBCR data by species
#' 
#' This function summarizes the spatial IMBCR object from `get_imbcr()` by 
#'     species. The summary includes the number of records per species, minimum 
#'     and maximum year a species is observed, and the IMBCR record ID if there 
#'     are less than seven (7) observations. This function then verifies 
#'     taxonomy using the `get_taxonomies()` function.
#'
#' @param imbcr_data Spatial IMBCR data from `get_imbcr()`.
#'
#' @return A [tibble::tibble()].
#' @seealso [get_imbcr()], [get_taxonomies()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Read IMBCR data into R
#' mgmt_units <- c("Cimarron National Grassland", "Comanche National Grassland")
#' imbcr_dat <- get_imbcr(mgmt_units)
#' 
#' # Summarize data by species
#' imbcr_spp(imbcr_dat)
#' 
#' ## End(Not run)                     
imbcr_spp <- function(imbcr_data){
  locale = stringr::str_c(unique(imbcr_data$locale), collapse = ", ")
  taxa_dat = sf::st_drop_geometry(imbcr_data) |>
    dplyr::select(scientific_name, Species, BirdCode, order:species_group, 
                  locale) |> 
    dplyr::distinct(scientific_name, .keep_all = TRUE) |> 
    dplyr::rename("common_name_IMBCR" = Species, 
                  "order_eBird" = order, 
                  "family_eBird" = family, 
                  "locale" = locale)
  spp_stats = sf::st_drop_geometry(imbcr_data) |>
    dplyr::select(scientific_name, Year) |> 
    dplyr::group_by(scientific_name) |> 
    dplyr::summarize(nObs = dplyr::n(), 
                     minYear = min(Year, na.rm = TRUE), 
                     maxYear = max(Year, na.rm = TRUE), 
                     .groups = "drop")
  dat = dplyr::left_join(spp_stats, taxa_dat, by = "scientific_name") |> 
    dplyr::mutate(source = "IMBCR") |> 
    mpsgSE::get_taxonomies(query_field = "scientific_name") |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}


#' Compile IMBCR list
#' 
#' Compile a comprehensive species list from IMBCR occurrence records on Forest 
#'     Service (FS) land and a 1-km buffer of FS land. This function uses the 
#'     `imbcr_spp()` function on two clipped spatial objects from `get_imbcr()`.
#'
#' @param imbcr_unit Spatial IMBCR data from `get_imbcr()` clipped to FS land.
#' @param imbcr_buff Spatial IMBCR data from `get_imbcr()` clipped to the 1-km 
#'                    buffer of FS land.
#'
#' @return A tibble.
#' @seealso [get_imbcr()], [imbcr_spp()], [clip_fc()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Read spatial data into R
#' t_path <- file.path("T:/path/to/project/directory")
#' gdb_path <- file.path(t_path, "GIS_Data.gdb")
#' sf_fs <- read_fc(lyr = "PlanArea", dsn = gdb_path, crs = "NAD83")
#' sf_buff <- read_fc(lyr = "PlanArea_1kmBuffer", dsn = gdb_path, crs = "NAD83")
#' 
#' # Read IMBCR data into R
#' mgmt_units <- c("Cimarron National Grassland", "Comanche National Grassland")
#' imbcr_dat <- get_imbcr(mgmt_units)
#' 
#' # Clip to extents
#' unit_imbcr <- clip_fc(imbcr_dat, sf_fs)
#' buff_imbcr <- clip_fc(imbcr_dat, sf_buff)
#' 
#' compile_imbcr_list(unit_imbcr, buff_imbcr)
#' 
#' ## End(Not run)                     
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
