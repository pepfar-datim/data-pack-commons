require(magrittr)

require(datapackcommons)
require(assertthat)
require(datimutils)
require(tidyr)
require(dplyr)

#' @title ValidateDimItems
#' @description Compares dimension item uids and names from the configuration file
#' to what is in DATIM. Discrepancies are returned. There should not be any
#' discrepancies in a properly configured configuration file. 
#' @param data configuration details for DataPack or PSNUxIM model
#' @param dim_uid_colname name of column in data with DATIM dimension uids
#' @param item_name_colname name of column in data with DATIM dimension item names
#' @param item_uid_colname name of column in data with DATIM dimension item uids
#' @return TRUE if no issues found, otherwise error 

ValidateDimItems <-
  function(data, dim_uid_colname, item_name_colname, item_uid_colname) {
    # validate dimension item names and ids
    # 1 - Get items for all dimensions listed
    api_names_ids <- data[[dim_uid_colname]] %>% 
      unique() %>% na.omit() %>%
      paste0(collapse = ",") %>% {paste0("id:in:[", ., "]")} %>%
      {datapackcommons::getMetadata("dimensions", filters = .,
                                     fields = "items[name,id]")} %>%
      tidyr::unnest(c("items")) 
    
    # 2 - compare api items with items in data
    unmatched <- data %>% dplyr::select(!!item_uid_colname, !!item_name_colname) %>%
      setNames(c("id", "name")) %>% 
      na.omit() %>% unique() %>%
      dplyr::anti_join(api_names_ids) 
    
    if(NROW(unmatched) > 0){
      stop("Dimension items that cannot be matched", unmatched)
    } else{
      TRUE
    }
  }  

#' @title ValidateDimItemSets
#' @description Internal validation of dimension_item_sets configuration. Checks
#' the dimension, dimension item, category option, and category option combo uids and names 
#' match what is in DATIM
#' @param dim_item_sets the dimension configuration to validate, 
#' generally read in from CSV
#' @return nothing returned, but if errors in configuration are detected
#' an error is thrown

ValidateDimItemSets <- function(dim_item_sets){
  
# validate dimension names and ids
  dim_item_sets %>% dplyr::filter(!is.na(dim_uid), dim_uid != "co") %>% 
  {datapackcommons::ValidateNameIdPairs(.$dim_name, .$dim_uid, "dimensions")} %>% 
    assertthat::assert_that()

# validate dimension item names and ids
  dim_item_sets %>% dplyr::filter(dim_uid != "co") %>%
  ValidateDimItems("dim_uid", "dim_item_name", "dim_item_uid")
  
# validate category option names and ids
  dim_item_sets %>% dplyr::filter(!is.na(.$option_uid)) %>%  
  {datapackcommons::ValidateNameIdPairs(.$option_name, .$option_uid, "categoryOptions")} %>% 
    assertthat::assert_that()  

  # validate category option combination rows
  dim_item_sets %>% dplyr::filter(dim_uid == "co") %>%
    .$dim_name %>% unique() %>% assertthat::are_equal("Category option combo") %>% 
    assertthat::assert_that()  
  # category option combination names and ids
  dim_item_sets %>%  dplyr::filter(dim_uid == "co") %>%
  {datapackcommons::ValidateNameIdPairs(.$dim_item_name, .$dim_item_uid, "categoryOptionCombos")} %>% 
    assertthat::assert_that()  
  
  }

#' @title ValidateDataRequired
#' @description Internal validation of data_required configuration. Checks
#' the analytics data dimension name and id match what is in DATIM
#' @param data_required data_required configuration typically read in from CSV
#' @return nothing returned, but if errors in configuration are detected
#' an error is thrown

ValidateDataRequired <- function(data_required){
  
#TODO  we have drifted from only using indicators, should this be updated
# to check other data types such as dataElements?
  data_required %>% dplyr::filter(!is.na(A.dx_name)) %>%
  {datapackcommons::ValidateNameIdPairs(.[["A.dx_name"]], 
                                        .[["A.dx_id"]], 
                                        "indicators")} %>% 
    assertthat::assert_that()
  
  data_required %>% dplyr::filter(!is.na(B.dx_name)) %>%
  {datapackcommons::ValidateNameIdPairs(.[["B.dx_name"]], 
                                        .[["B.dx_id"]], 
                                        "indicators")} %>% 
    assertthat::assert_that()
  
}

#' @title ValidateMapT_1toT
#' @description  Internal validation of PSNUxIM configuration map. Checks
#' the analytics data dimension name and id match what is in DATIM
#' @param t_1_to_t PSNUxIM configuration mapping to validate. Usually read 
#' in from a CSV
#' @param dim_item_sets the dimension configuration to use for validation, 
#' generally read in from CSV
#' @return nothing returned, but if errors in configuration are detected
#' an error is thrown

ValidateMapT_1toT <- function(t_1_to_t, dim_item_sets){
  
# Checking technical area dimension items
  t_1_to_t %>% mutate(dim_uid_colname = "LxhLO68FcXm") %>% 
  ValidateDimItems("dim_uid_colname", "technical_area", "technical_area_uid") 
  
# Checking numerator/denominator dimension items
  t_1_to_t %>% mutate(dim_uid_colname = "lD2x0c8kywj") %>% 
    ValidateDimItems("dim_uid_colname", "num_or_den", "num_or_den_uid") 
  
# Checking Disaggregation Type
# OVC_SERV has a mapping to multiple historic disaggregation types which
# are separated by a semi-colon
  t_1_to_t %>% mutate(dim_uid_colname = "HWPJnUTMjEq") %>% 
    dplyr::mutate(disagg_type= 
                    stringr::str_split(disagg_type, pattern = ";"),
                  disagg_type_uid= 
                    stringr::str_split(disagg_type_uid, pattern = ";")) %>% 
    tidyr::unnest(disagg_type, disagg_type_uid) %>% 
    ValidateDimItems("dim_uid_colname", "disagg_type", "disagg_type_uid") 
  
# check for matching model sets in Dimension item sets
  c(t_1_to_t$age_set, 
    t_1_to_t$sex_set, 
    t_1_to_t$kp_set, 
    t_1_to_t$other_disagg) %>% 
    na.omit() %>% 
    {. %in% dim_item_sets$model_sets} %>% 
    all() %>% 
    assertthat::assert_that()

  }

datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "datim.json"))

wd <- getwd()
setwd("~/Documents/GitHub/data-pack-commons")

dim_item_sets <- readr::read_csv("./data-raw/model_calculations/dimension_item_sets.csv",
                                 col_types = readr::cols(.default = "c", sort_order = "d", weight = "d"),
                                 na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
                dim_item_name, option_name, option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)

ValidateDimItemSets(dim_item_sets)

data_required <- 
  readr::read_csv("./data-raw/model_calculations/data_required.csv", 
                  col_types = readr::cols(.default = "c", A.value_na = "d", B.value_na = "d"),
                  na = c("NA")) %>% select(-data_pack_type)

ValidateDataRequired(data_required)

Map22Tto23T <- 
  readr::read_csv("./data-raw/snu_x_im_distribution_configuration/22Tto23TMap.csv", 
                  col_types = readr::cols(.default = "c"),
                  na = c("NA")) 

ValidateMapT_1toT(Map22Tto23T, dim_item_sets)

dplyr::all_equal(datapackcommons::data_required, data_required)
dplyr::all_equal(datapackcommons::Map22Tto23T, Map22Tto23T)
dplyr::all_equal(datapackcommons::dim_item_sets, dim_item_sets)
dr_dif_removed <- dplyr::anti_join(datapackcommons::data_required, data_required)
map_dif_removed <- dplyr::anti_join(datapackcommons::Map22Tto23T, Map22Tto23T)
dim_dif_removed <- dplyr::anti_join(datapackcommons::dim_item_sets, dim_item_sets)
dr_dif_added <- dplyr::anti_join(data_required, 
                                 datapackcommons::data_required)
map_dif_added <- dplyr::anti_join(Map22Tto23T, 
                                  datapackcommons::Map22Tto23T)
dim_dif_added <- dplyr::anti_join(dim_item_sets,
                                  datapackcommons::dim_item_sets)

usethis::use_data(dim_item_sets, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required, overwrite = TRUE, compress = "gzip")
usethis::use_data(Map22Tto23T, overwrite = TRUE, compress = "gzip")

setwd(wd)