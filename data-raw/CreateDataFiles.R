require(tidyverse)
require(datapackcommons)
require(assertthat)

ValidateDimItems <-
  function(data, dim_uid_colname, item_name_colname, item_uid_colname, base_url) {
    # validate dimension item names and ids
    # 1 - Get items for all dimensions listed
    api_names_ids <- data[[dim_uid_colname]] %>% 
      unique() %>% na.omit() %>%
      paste0(collapse = ",") %>% {paste0("id:in:[", ., "]")} %>%
      {datapackcommons::getMetadata(base_url, "dimensions", filters = .,
                                     fields = "items[name,id]")} %>%
      unnest() 
    
    # 2 - compare api items with items in data
    unmatched <- data %>% dplyr::select(item_uid_colname, item_name_colname) %>%
      setNames(c("id", "name")) %>% 
      na.omit() %>% unique() %>%
      dplyr::anti_join(api_names_ids) 
    
    if(NROW(unmatched) > 0){
      stop("Dimension items that cannot be matched", unmatched)
    } else{
      TRUE
    }
  }  

ValidateDimItemSets <- function(dim_item_sets, base_url){
  
# validate dimension names and ids
  dim_item_sets %>% dplyr::filter(!is.na(dim_uid)) %>% 
  {datapackcommons::ValidateNameIdPairs(base_url, .$dim_name, .$dim_uid, "dimensions")} %>% 
    assertthat::assert_that()
  
# validate dimension item names and ids
  
  ValidateDimItems(dim_item_sets, "dim_uid", "dim_item_name", "dim_item_uid", base_url)
  
# validate category option names and ids
  dim_item_sets %>%  
  {datapackcommons::ValidateNameIdPairs(base_url, .$option_name, .$option_uid, "categoryOptions")} %>% 
    assertthat::assert_that()  
}

ValidateDataRequired <- function(data_required, base_url){
  data_required %>% dplyr::filter(!is.na(A.ind_uid)) %>%
  {ValidateCodeIdPairs(base_url, .[["A.ind_code"]], .[["A.ind_uid"]], "indicators")}
  data_required %>% dplyr::filter(!is.na(B.ind_uid)) %>%
  {ValidateCodeIdPairs(base_url, .[["B.ind_code"]], .[["B.ind_uid"]], "indicators")}
  
  data_required %>% dplyr::filter(!is.na(A.add_dim_1_uid)) %>%
  {ValidateNameIdPairs(base_url, .[["A.add_dim_1"]], .[["A.add_dim_1_uid"]], "dimensions")}
  data_required %>% dplyr::filter(!is.na(B.add_dim_1_uid)) %>%
  {ValidateNameIdPairs(base_url, .[["B.add_dim_1"]], .[["B.add_dim_1_uid"]], "dimensions")}
  
  data_required %>% ValidateDimItems("A.add_dim_1_uid", "A.add_dim_1_items", "A.add_dim_1_items_uid", base_url)
  data_required %>% ValidateDimItems("B.add_dim_1_uid", "B.add_dim_1_items", "B.add_dim_1_items_uid", base_url)
}

datapackcommons::DHISLogin("/users/sam/.secrets/prod.json")
base_url <- getOption("baseurl")

dim_item_sets <- readr::read_csv("./data-raw/model_calculations/dimension_item_sets.csv",
                                 col_types = readr::cols(.default = "c", sort_order = "d", weight = "d"),
                                 na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
                dim_item_name, option_name, option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)

ValidateDimItemSets(dim_item_sets, base_url)

dim_item_sets_test <- readr::read_csv("./data-raw/model_calculations_test/dimension_item_sets.csv",
                                      col_types = readr::cols(.default = "c", sort_order = "d", weight = "d"),
                                      na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
                dim_item_name, option_name, option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)
 

data_required <- 
  readr::read_csv("./data-raw/model_calculations/data_required.csv", 
                  col_types = readr::cols(.default = "c"),
                  na = c("NA")) %>%
  dplyr::mutate(B.kp_set = NA_character_) %>% select(-data_pack_type)

ValidateDataRequired(data_required, base_url)

data_required_test <- 
  readr::read_csv("./data-raw/model_calculations_test/data_required.csv",
                  col_types = readr::cols(.default = "c"),
                  na = c("NA")) %>%
  dplyr::mutate(B.kp_set = NA_character_) %>% select(-data_pack_type)

wd <- getwd()
setwd("/Users/sam/Documents/GitHub/data-pack-commons")

usethis::use_data(dim_item_sets, overwrite = TRUE, compress = "gzip")
usethis::use_data(dim_item_sets_test, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required_test, overwrite = TRUE, compress = "gzip")
setwd(wd)