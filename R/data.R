#' Core Index data from the Breeding Bird Survey (USGS) 1966-2023
#'
#' @format A data frame of 988,146 observations and 6 variables.
#' \describe{
#'   \item{aou}{BBS 5 digit codes that represent bird species.}
#'   \item{region}{BBS region code.}
#'   \item{year}{4 digit year.}
#'   \item{index}{Annual indexes (Index) are relative abundance estimates for all years, estimated as yearly predicted abundances from the hierarchical model analysis.}
#'   \item{ci_2.5_percenti}{2.5 percent confidence interval.}
#'   \item{ci_97.5_percent}{97.5 percent confidence interval}
#' }
#' @source https://www.sciencebase.gov/catalog/item/67aba702d34e329fb20457b1
"core_index"


#' Core Trend Data from the Breeding Bird Survey (USGS) 1966-2023
#'
#' @format A data frame of 17,037 observations and 16 variables.
#' \describe{
#'   \item{credibility_code}{BBS model credibility code. R is a Red Indicator reflecting an impotrant deficiency in the model and Y is a Yellow Indicator reflecing a deficiency in the model.}
#'   \item{sample_size_code}{Sample size component of the credibility code. R is based on less than 5 routes. Y is based on less than 14 routes. NA is based on at lease 14 routes.}
#'   \item{precision_code}{Precision component to the credibility code. R is very imprecise. Y is quite imprecise. NA is precise.}
#'   \item{abundance_code}{Abundance component to the credibility code. R is very low abundance. Y is low abundance. NA is abundant.}
#'   \item{significance}{An indicator variable that takes on value "1" if zero falls outside the 95% credible interval for the estimate and the trend estimate is > 0; "2" if zero falls outside the 95% credible interval for the estimate and the trend estimate is < 0; and " " (no value) if zero falls within the 95% credible interval. If the credible interval does not contain 0, the result could be judged significant.}
#'   \item{aou}{BBS 5 digit codes that represent bird species.}
#'   \item{region}{BBS region code.}
#'   \item{species}{Common name of species.}
#'   \item{region_name}{BBS region name.}
#'   \item{model}{The statistical model used for the species.}
#'   \item{n_routes}{The number of survey routes on which the species was encountered during the interval.}
#'   \item{trend}{Trend estimates are calculated as the ratio of endpoints from the annual indices produced in the hierarchical model analysis and represent yearly percent change.}
#'   \item{x2_5_percent_ci}{2.5 percent confidence interval.}
#'   \item{x97_5_percent_ci}{9y.5 percent confidence interval.}
#'   \item{relative_abundance}{The annual index for the region at the midpoint of the interval.}
#'   \item{years}{Range of years that this trend covers.}
#' }
#' @source https://www.sciencebase.gov/catalog/item/67aba702d34e329fb20457b1
"core_trend"

#' Expanded Index data from the Breeding Bird Survey (USGS) 1993-2023
#'
#' @format A data frame of 612,622 observations and 6 variables.
#' \describe{
#'   \item{aou}{BBS 5 digit codes that represent bird species.}
#'   \item{region}{BBS region code.}
#'   \item{year}{4 digit year.}
#'   \item{index}{Annual indexes (Index) are relative abundance estimates for all years, estimated as yearly predicted abundances from the hierarchical model analysis.}
#'   \item{ci_2.5_percenti}{2.5 percent confidence interval.}
#'   \item{ci_97.5_percent}{97.5 percent confidence interval}
#' }
#' @source https://www.sciencebase.gov/catalog/item/67aba702d34e329fb20457b1
"expanded_index"



#' Expanded Trend Data from the Breeding Bird Survey (USGS), 1993-2023
#'
#' @format A data frame of 19,762 observations and 15 variables.
#' \describe{
#'   \item{credibility_code}{BBS model credibility code. R is a Red Indicator reflecting an impotrant deficiency in the model and Y is a Yellow Indicator reflecing a deficiency in the model.}
#'   \item{sample_size_code}{Sample size component of the credibility code. R is based on less than 5 routes. Y is based on less than 14 routes. NA is based on at lease 14 routes.}
#'   \item{precision_code}{Precision component to the credibility code. R is very imprecise. Y is quite imprecise. NA is precise.}
#'   \item{abundance_code}{Abundance component to the credibility code. R is very low abundance. Y is low abundance. NA is abundant.}
#'   \item{significance}{An indicator variable that takes on value "1" if zero falls outside the 95% credible interval for the estimate and the trend estimate is > 0; "2" if zero falls outside the 95% credible interval for the estimate and the trend estimate is < 0; and " " (no value) if zero falls within the 95% credible interval. If the credible interval does not contain 0, the result could be judged significant.}
#'   \item{aou}{BBS 5 digit codes that represent bird species.}
#'   \item{region}{BBS region code.}
#'   \item{species}{Common name of species.}
#'   \item{region_name}{BBS region name.}
#'   \item{model}{The statistical model used for the species.}
#'   \item{n_routes}{The number of survey routes on which the species was encountered during the interval.}
#'   \item{trend}{Trend estimates are calculated as the ratio of endpoints from the annual indices produced in the hierarchical model analysis and represent yearly percent change.}
#'   \item{x2_5_percent_ci}{2.5 percent confidence interval.}
#'   \item{x97_5_percent_ci}{9y.5 percent confidence interval.}
#'   \item{relative_abundance}{The annual index for the region at the midpoint of the interval.}
#' }
#' @source https://www.sciencebase.gov/catalog/item/67aba702d34e329fb20457b1
"expanded_trend"


#' 2024 US Fish & Wildlife Birds of Conservation Concern List
#' 
#' @format A data frame of 333 observations and 17 variables.
#' \describe{
#'   \item{common_name}{Common name of bird species.}
#'   \item{scientific_name}{Scientific name of bird species.}
#'   \item{mbta}{Migratory Bird Treaty Act designation.}
#'   \item{federally_endangered}{Location description of where the species is Endangered.}
#'   \item{federally_threatened}{Location description of where the species is Threatened.}
#'   \item{bcc_rangewide_in_continental_us_and_or_pr_and_vi_or_hi_and_pacific_islands}{description}
#'   \item{bc_rs_for_bcc_listing_non_breeding}{List of Bird Conservation Region codes for non-breeding birds.}
#'   \item{bc_rs_for_bcc_listing_breeding}{List of Bird Conservation Region codes for breeding birds.}
#'   \item{gbif_taxonID}{GBIF taxon ID}
#'   \item{kingdom}{Taxonomic Kingdom}
#'   \item{phylum}{Taxonomic Phylum}
#'   \item{class}{Taxonomic Class}
#'   \item{order}{Taxonomic Order}
#'   \item{family}{Taxonomic Family}
#'   \item{genus}{Taxonomic Genus}
#'   \item{species}{Taxonomic Species}
#'   \item{taxon_id}{MPSG taxon ID.}
#' }
#' @source https://www.fws.gov/media/usfws-bird-species-concern
"bcc_list"


#'  North American Bird Conservation Initiative Birds Conservation Regions
#' 
#' @format An `sf` object (polygon) of 4414 observations and 9 variables.
#' \describe{
#'   \item{bcr_label}{Birds Conservation Region codes.}
#'   \item{bcr_label_name}{Birds Conservation Regions names.}
#'   \item{name_en}{Birds Conservation Regions names in English.}
#'   \item{name_fr}{Birds Conservation Regions names in French.}
#'   \item{name_sp}{Birds Conservation Regions names in Spanish}
#'   \item{globalid}{Polygon ID.}
#'   \item{SHAPE_Length}{Polygon length.}
#'   \item{SHAPE_Area}{Polygon area.}
#'   \item{SHAPE}{Polygon geometry.}
#' }
#' @source https://www.birdscanada.org/bird-science/nabci-bird-conservation-regions
"bcc_regions"


#' Conservation lists in standardized format.  All lists have a taxon_id based 
#'     on the GBIF accepted taxonomy. Lists so far included are:
#' * Colorado SWAP
#' * Colorado T and E
#' * USFS Region 2 Sensitive Species
"conservation_lists"


#' List of Forest Service Units
#' 
#' @format A data frame
#' @source Forest Service EDW Rest Services, https://apps.fs.usda.gov/arcx/rest/services/EDW.
"fs_units"


#' Idaho SWAP List
#' 
#' `id_swap` Idaho SWAP list from 2023.
"id_swap"


#' Montana SWAP List
#' 
#' `mt_swap` Montana SWAP list from 2023.
"mt_swap"


#' Regional Forester's Sensitive Species List for Regions 1-10
#' 
#' `rfss` is a comprehensive master list from 2024.
#' `rfss_taxonomy` is the taxonomic classifications returned form GBIF.
"rfss"
"rfss_taxonomy"


#' An example dataset to test functions that consists of a list of species with 
#' common_name and scientific_name.
#'
"sp_list_ex"


#' Utah BLM Sensitive Species List
#' 
#' `ut_blm_ss` Utah BLM Sensitive Species list from 2025.
"ut_blm_ss"


#' Utah SWAP List
#' 
#' `ut_swap` Utah SWAP list from 2025.
"ut_swap"
