#' Subset eligible species from IMBCR data. 
#'
#' @param imbcr_data Spatial IMBCR data from [get_imbcr_data()].
#' @param spp_list Species list that includes taxon ID from [get_taxonomies()]. 
#'                     This is the list that is used to subset the spatial data.
#'
#' @return An [sf] object.
#' 
#' @details
#' Additional details...
#' 
#' @seealso [get_imbcr_data()], [build_imbcr_spp()], [get_taxonomies()]
#' 
#' @export
#' 
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Read IMBCR data into R
#' mgmt_units <- c("Cimarron National Grassland", "Comanche National Grassland")
#' imbcr_dat <- get_imbcr_data(mgmt_units)
#' # Summarize data by species
#' spp_list <- build_imbcr_spp(imbcr_dat)
#' birds <- dplyr::filter(spp_list, family == "Corvidae")
#' # Subset spatial data
#' corvids <- build_imbcr_spatial_data(imbcr_dat, spp_list, birds)
#' 
#' ## End(Not run)
build_imbcr_spatial_data <- function(imbcr_data, spp_list){
  
  # imbcr_data = targets::tar_read(imbcr_unit)
  # spp_list = targets::tar_read(elig_list)
  
  # Filter spatial data
  elig_imbcr = imbcr_data |> 
    dplyr::filter(taxon_id %in% spp_list$taxon_id)
  
  return(elig_imbcr)
}


#' Read IMBCR Data into R.
#' 
#' This function reads IMBCR data into R. This function references restricted 
#'     data on the MPSG T-drive and will fail if the user is not on a Forest 
#'     Service network or the VDI and does not have access to the restricted data 
#'     folder.
#'
#' @param fs_unit The IMBCR management unit. Use `see_imbcr_mgmt_units()` to 
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
#' get_imbcr_data(mgmt_units)
#' 
#' ## End(Not run)                     
get_imbcr_data <- function(fs_unit, crs = NULL, dir_path = NULL){
  # library('sf')
  # fs_unit = "Medicine Bow-Routt National Forests"
  # crs = "EPSG:26913"
  
  # Define file path
  if(is.null(dir_path)){
    dir_path = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species/IMBCR")
  }

  # IMBCR Data ----
  message("Reading IMBCR data")
  dat = readRDS(file.path(dir_path, "imbcr_data.RDS")) |>
    dplyr::filter(mgmt_unit %in% fs_unit) |> 
    dplyr::filter(!stringr::str_detect(species, "Unknown"))
  
  # Re-project CRS
  if(!is.null(crs)){
    if(sf::st_crs(dat) != crs) dat = sf::st_transform(dat, crs = crs)
  }
  
  # Return data
  return(dat)
}


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


#' List of IMBCR Management Units on National Forests and Grasslands
#'
#' @format ## `imbcr_mgmt_units`
"imbcr_mgmt_units"


#' Summarize IMBCR data by species
#' 
#' This function summarizes the spatial IMBCR object from `get_imbcr_data()` by 
#'     species. The summary includes the number of records per species, minimum 
#'     and maximum year a species is observed, and taxonomy from the GBIF 
#'     backbone taxonomy. This function references restricted data on the 
#'     MPSG T-drive and will fail if the user is not on a Forest Service network
#'     or the VDI and does not have access to the restricted data folder.
#'
#' @param imbcr_data Spatial IMBCR data from `get_imbcr_data()`.
#' @param dir_path Path to the IMBCR data folder. Default is NULL. If NULL, data 
#'                     will be read from the MPSG T-drive folder.
#'
#' @return A [tibble::tibble()].
#' @seealso [get_imbcr_data()], [get_taxonomies()]
#' @export
#'
#' @examples
#' ## Not run:
#' 
#' library("mpsgSE")
#' 
#' # Read IMBCR data into R
#' mgmt_units <- c("Cimarron National Grassland", "Comanche National Grassland")
#' imbcr_dat <- get_imbcr_data(mgmt_units)
#' 
#' # Summarize data by species
#' build_imbcr_spp(imbcr_dat)
#' 
#' ## End(Not run)
build_imbcr_spp <- function(imbcr_data, dir_path = NULL){
  # imbcr_data = targets::tar_read(imbcr_unit)
  # Get location
  locale = stringr::str_c(unique(imbcr_data$locale), collapse = ", ")
  
  # Read IMBCR Taxonomy
  if(is.null(dir_path)){
    dir_path = file.path("T:/FS/NFS/PSO/MPSG/MPSG_Restricted/Species/IMBCR")
  }
  taxonomy = sf::st_drop_geometry(imbcr_data)|> 
    dplyr::select(taxon_id, species) |>
    dplyr::rename("common_name" = species) |> 
    dplyr::distinct() |> 
    dplyr::left_join(readRDS(file.path(dir_path, "imbcr_taxonomy.RDS")), 
                     by = "taxon_id", relationship = 'many-to-many')
  
  # Calculate species stats
  spp_stats = sf::st_drop_geometry(imbcr_data) |>
    dplyr::select(taxon_id, year) |> 
    dplyr::group_by(taxon_id) |> 
    dplyr::summarize(nObs = dplyr::n(), 
                     minYear = min(year, na.rm = TRUE), 
                     maxYear = max(year, na.rm = TRUE), 
                     .groups = "drop")
  
  # Assemble final data frame
  variable_order = c("taxon_id", "scientific_name", "common_name", "bird_code", 
                     "nObs", "minYear", "maxYear", "locale", "source", 
                     "gbif_taxonID", "kingdom", "phylum", "class", "order", 
                     "family", "genus", "species", "subspecies", "variety", 
                     "form")
  dat = dplyr::left_join(spp_stats, taxonomy, by = "taxon_id") |> 
    dplyr::mutate(source = "IMBCR", locale = locale) |> # colnames() |> sort()
    dplyr::select(dplyr::any_of(variable_order)) |> 
    dplyr::arrange(kingdom, phylum, class, order, family, genus, 
                   species, scientific_name)
  return(dat)
}
