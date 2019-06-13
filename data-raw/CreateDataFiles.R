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
  dim_item_sets %>% dplyr::filter(!is.na(dim_uid), dim_uid != "co") %>% 
  {datapackcommons::ValidateNameIdPairs(.$dim_name, .$dim_uid, "dimensions")} %>% 
    assertthat::assert_that()

# validate dimension item names and ids
  dim_item_sets %>% dplyr::filter(dim_uid != "co") %>%
  ValidateDimItems("dim_uid", "dim_item_name", "dim_item_uid", base_url)
  
# validate category option names and ids
  dim_item_sets %>%  
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

ValidateDataRequired <- function(data_required, base_url){
  data_required %>% dplyr::filter(!is.na(A.ind_uid)) %>%
  {ValidateCodeIdPairs(base_url, .[["A.ind_code"]], .[["A.ind_uid"]], "indicators")}
  data_required %>% dplyr::filter(!is.na(B.ind_uid)) %>%
  {ValidateCodeIdPairs(base_url, .[["B.ind_code"]], .[["B.ind_uid"]], "indicators")}
  
  data_required %>% dplyr::filter(!is.na(A.add_dim_1_uid)) %>%
  {ValidateNameIdPairs(.[["A.add_dim_1"]], .[["A.add_dim_1_uid"]], "dimensions")}
  data_required %>% dplyr::filter(!is.na(B.add_dim_1_uid)) %>%
  {ValidateNameIdPairs(.[["B.add_dim_1"]], .[["B.add_dim_1_uid"]], "dimensions")}
  
  data_required %>% ValidateDimItems("A.add_dim_1_uid", "A.add_dim_1_items", "A.add_dim_1_items_uid", base_url)
  data_required %>% ValidateDimItems("B.add_dim_1_uid", "B.add_dim_1_items", "B.add_dim_1_items_uid", base_url)
}

ValidateMap19Tto20T <- function(Map19Tto20T, dim_item_sets, base_url){
  
  Map19Tto20T %>% mutate(dim_uid_colname = "LxhLO68FcXm") %>% 
  ValidateDimItems("dim_uid_colname", "technical_area", "technical_area_uid", base_url) 
  
  Map19Tto20T %>% mutate(dim_uid_colname = "lD2x0c8kywj") %>% 
    ValidateDimItems("dim_uid_colname", "num_or_den", "num_or_den_uid", base_url) 
      
  Map19Tto20T %>% mutate(dim_uid_colname = "HWPJnUTMjEq") %>% 
    ValidateDimItems("dim_uid_colname", "disagg_type", "disagg_type_uid", base_url) 
  
# chack for matching model sets in Dimension item sets
  c(Map19Tto20T$age_set, Map19Tto20T$sex_set, Map19Tto20T$kp_set, Map19Tto20T$other_disagg) %>% 
    na.omit() %>% 
    {. %in% dim_item_sets$model_sets} %>% 
    all() %>% 
    assertthat::assert_that()

  }

datapackcommons::DHISLogin("/users/sam/.secrets/prod.json")
base_url <- getOption("baseurl")
wd <- getwd()
setwd("/Users/sam/Documents/GitHub/data-pack-commons")

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

Map19Tto20T <- 
  readr::read_csv("./data-raw/site_distribution_configuration/19Tto20TMap.csv", 
                  col_types = readr::cols(.default = "c"),
                  na = c("NA")) 

# MER Targets: Community Based FY2019 data set = l796jk9SW7q
# Get character vector of indicator names
community_indicators_19T <-  datapackcommons::getMetadata(base_url, end_point = "dataSets", 
                             filters = "id:eq:l796jk9SW7q", 
                             fields = "dataSetElements[dataElement[name]]") %>% 
  .[["dataSetElements"]] %>% 
  .[[1]] %>% 
  .[["dataElement"]] %>% 
  .[["name"]] 

# check if historic indicator in the map is a community indicator 
# handle the no disagg case seperately
is_community_indicator <- 
  dplyr::transmute(Map19Tto20T, 
                   reg_exp = dplyr::if_else(disagg_type == "No Disagg",
                                            paste0(technical_area, " \\(",
                                                   stringr::str_sub(num_or_den,1,1), ", DSD\\)"),
                                            paste0(technical_area, " \\(",
                                                   stringr::str_sub(num_or_den,1,1), ", DSD, ", 
                                                   disagg_type, "\\)")
                                            )
                   ) %>% 
  .[["reg_exp"]] %>% 
  purrr::map_int(~sum(grepl(., community_indicators_19T)))

if(any(is_community_indicator > 1)){
  stop("Getting more than one match between community data set indicators and mapped historic indicator")
}

# MER Targets: Facility Based FY2019 data set = eyI0UOWJnDk
# Get character vector of indicator names
facility_indicators_19T <-  datapackcommons::getMetadata(base_url, end_point = "dataSets", 
                                                          filters = "id:eq:eyI0UOWJnDk",
                                                          fields = "dataSetElements[dataElement[name]]") %>% 
  .[["dataSetElements"]] %>% 
  .[[1]] %>% 
  .[["dataElement"]] %>% 
  .[["name"]] 

# check if historic indicator is facility indicator 
# handle the no disagg case seperatly
is_facility_indicator <- 
  dplyr::transmute(Map19Tto20T, 
                   reg_exp = dplyr::if_else(disagg_type == "No Disagg",
                                            paste0(technical_area, " \\(",
                                                   stringr::str_sub(num_or_den,1,1), ", DSD\\)"),
                                            paste0(technical_area, " \\(",
                                                   stringr::str_sub(num_or_den,1,1), ", DSD, ", 
                                                   disagg_type, "\\)")
                   )
  ) %>% 
  .[["reg_exp"]] %>% 
  purrr::map_int(~sum(grepl(., facility_indicators_19T)))

if(any(is_facility_indicator > 1)){
  stop("Getting more than one match between facility data set indicators and mapped historic indicator")
}

Map19Tto20T <- mutate(Map19Tto20T, 
                      community_valid = as.logical(is_community_indicator),
                      facility_valid = as.logical(is_facility_indicator)
                      )

ValidateMap19Tto20T(Map19Tto20T, dim_item_sets, base_url)



regional_to_national_mechs_cop19 <- readr::read_csv("./data-raw/NextGen Split Regional Mechanisms.csv",
                                 col_types = readr::cols(.default = "c"))


usethis::use_data(dim_item_sets, overwrite = TRUE, compress = "gzip")
usethis::use_data(dim_item_sets_test, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required_test, overwrite = TRUE, compress = "gzip")
usethis::use_data(Map19Tto20T, overwrite = TRUE, compress = "gzip")
usethis::use_data(regional_to_national_mechs_cop19, overwrite = TRUE, compress = "gzip")

setwd(wd)