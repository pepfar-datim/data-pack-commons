# devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
#                   build = TRUE,
#                   upgrade = FALSE)

 require(datapackcommons)
# require(tidyverse)
# require(jsonlite)
# require(lubridate)
# require(rlang)
# require(assertthat)
# require(foreach)
# # require(rlist)
# 
 require(datapackcommons)
 require(tidyverse)
 DHISLogin("/users/sam/.secrets/triage.json")
 base_url <- getOption("baseurl")
 options(maxCacheAge = 0)
 #####
 
 SiteDensity <- function(datapack_data, data_element_map, dim_item_sets, country_details){
   
 } 
 
DistributeToSites <- function(datapack_data, data_element_map, dim_item_sets, mechanisms_historic_full, 
                              country_name){
  country_details <- datapackcommons::GetCountryLevels(base_url, country_name)
  
  country_uid = country_details$id
  
  mechanisms_uid <- dplyr::inner_join(mechanisms_historic_full, datimvalidation::getMechanismsMap(country_uid), 
                    by = c("uid" = "id")) %>% .$uid  
  
GetDensityDenom(country_uid, de_group, planning_level, technical_area, num_or_denom,
                  #' dissagg_type, period, mechanisms_uid, additional_dimensions = NULL, additional_filters = NULL,
                  #' base_url)
}

datapackcommons::DHISLogin("/users/sam/.secrets/prod.json")
base_url <- getOption("baseurl")
options(maxCacheAge = 0)
repo_path <- "/users/sam/Documents/GitHub/COP-19-Target-Setting/"
output_location <- "/Users/sam/COP data/"
datapack_export = NULL

mechanisms_19T <- Get19TMechanisms(base_url)
mechanisms_historic_full <- mechanisms_19T #temp

DistributeToSites(datapack_export, datapackcommons::Map19Tto20T, datapackcommons::dim_item_sets, mechanisms_19T,country_name)