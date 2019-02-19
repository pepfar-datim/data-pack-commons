
main <- function(){
  devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                    build = TRUE,
                    upgrade = FALSE)
  
  # require(datapackcommons)
  # require(tidyverse)
  # require(jsonlite)
  # require(lubridate)
  # require(rlang)
  # require(assertthat)
  # require(foreach)
  # # require(rlist)
  # 
  require(datapackcommons)
  require(datimvalidation)
  require(tidyverse)
  DHISLogin("/users/sam/.secrets/triage.json")
  base_url <- getOption("baseurl")
  options(maxCacheAge = 0)
  repo_path <- "/users/sam/Documents/GitHub/COP-19-Target-Setting/"
  output_location <- "/Users/sam/COP data/"
  datapack_export = NULL
  
  
  mechanisms_19T <- datapackcommons::Get19TMechanisms(base_url)
  mechanisms_historic_global <- mechanisms_19T #temp
  country_name <-  "Rwanda"
  de_group <- "zhdJiWlPvCz"
  period <- "2018Oct"

  DistributeToSites(datapack_export, datapackcommons::Map19Tto20T, de_group,
                    period, datapackcommons::dim_item_sets, mechanisms_19T, 
                    country_name)

}

DistributeToSites <- function(datapack_data, data_element_map, de_group,
                              period, dim_item_sets, mechanisms_historic_global, 
                              country_name){
  country_details <- datapackcommons::GetCountryLevels(base_url, country_name)
  country_uid = country_details$id
  
  
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_uid)
  
  assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, "uid"))
  assertthat::assert_that(assertthat::has_name(mechanisms_country, "id"))
  mechanisms_historic_country <- mechanisms_historic_global %>% 
    filter(uid %in% mechanisms_full_country$id)

# adply to call SiteDensity for each row of data_element_map
  doMC::registerDoMC(cores = 5) # or however many cores you have access to
  plyr::adply(data_element_map, 1, SiteDensity,..., .parallel = TRUE)
 }

SiteDensity <- function(data_element_map, dim_item_sets, de_group, country_details){
  country_uid <- country_details$id
  planning_level <- country_details$planning_level
  technical_area <- data_element_map$technical_area_uid
  num_or_den <- data_element_map$num_or_den_uid
  disagg_type <- data_element_map$disagg_type_uid
  age_set <- data_element_map$age_set
  sex_set <- data_element_map$sex_set
  kp_set <- data_element_map$kp_set
  other_disagg <- data_element_map$other_disagg
  
  GetDensityDenom(country_uid, de_group, planning_level, technical_area, num_or_den,
                  dissagg_type, period, mechanisms_uid, additional_dimensions = NULL, additional_filters = NULL,
                  base_url) 
  
#  sample output?
  SiteToolDensityMatrix <- tibble::tribble(~indicatorCode, ~Age, ~Sex, ~KeyPop, 
                                           ~mechanismCode, ~DsdOrTa, ~OrgUnitUid, 
                                           ~OrgUnitName, ~psnuid,  ~CommOrFac, 
                                           ~value,
                                           "HTS_INDEX_COM.N.Age/Sex/Result.20T.NewNeg", 
                                           "10-14", "Female", NA_character_, "18628", "DSD", "uid11111111", "An Org Unit", "uid22222222","Community", 11)
} 
