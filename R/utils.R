# title: "Functions for Species Occurrence Data Pulls"
# 
# This script holds functions for reading and clipping feature classes from 
#     geodatabases and pulling G-ranks from NatureServe.
# -----------------------------------------------------------------------------


#' Add Missing Columns
#' This function adds missing columns to one data frame that are     
#' @param df1 First data frame
#' @param df2 Second data frame
add_cols <- function(df1, df2){
  new_cols = colnames(df2)[!colnames(df2) %in% colnames(df1)]
  df1[new_cols] = NA
  return(df1)
}


#' Create vector from comma separated variable (column)
#' This function creates a vector from a comma separated variable or column, 
#'     then removes NAs and blanks 
#'
#' @param id_var Input variable, usually df$var (e.g., spp_list$GBIF_taxonKey) 
id_vec <- function(id_var){
  vec = stringr::strsplit(id_var, split = ", ") |> unlist() |> as.numeric()
  vec = vec[!is.na(vec)] # Remove NA's
  vec = vec[vec != ""]   # Remove blanks
}


#' View duplicate species in a data frame
#'
#' @param spp_dat Data frame or tibble of species data.
#' @param spp_vec Vector of species names from the same data frame.
view_dups <- function(spp_dat, spp_vec){
  dups <- spp_vec[duplicated(spp_vec)]
  spp_dat[spp_vec %in% dups, ] |> View()
  }


