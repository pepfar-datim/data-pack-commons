
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

# Takes data pack export and details for distributiong it and returns required output for site tool 
# import process
DistributeToSites <- function(datapack_data, data_element_map, de_group,
                              period, dim_item_sets, mechanisms_historic_global, 
                              country_name){
  country_details <- datapackcommons::GetCountryLevels(base_url, country_name)
  country_uid = country_details$id
  
  
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_uid)
  
  assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, "uid"))
  assertthat::assert_that(assertthat::has_name(mechanisms_full_country, "id"))
  mechanisms_historic_country <- mechanisms_historic_global %>% 
    filter(uid %in% mechanisms_full_country$id)

# adply to call SiteDensity for each row of data_element_map
# will have a distribution for each DSD/TA, site given psnu/IM
  doMC::registerDoMC(cores = 5) # or however many cores you have access to
  plyr::adply(data_element_map, 1, CalculateSiteDensity,..., .parallel = TRUE)
 }

# 
CalculateSiteDensity <- function(data_element_map, dim_item_sets, de_group, country_details, period){
  assertthat::assert_that(NROW(data_element_map) == 1.
                          NROW(country_details) == 1)

  country_uid <- country_details$id
  planning_level <- country_details$planning_level
  technical_area <- data_element_map$technical_area_uid
  num_or_den <- data_element_map$num_or_den_uid
  disagg_type <- data_element_map$disagg_type_uid

  disaggs <- c(data_element_map$age_set,
               data_element_map$sex_set,
               data_element_map$kp_set,
               data_element_map$other_disagg) %>% na.omit()
  
  if(length(disaggs) == 0){
    additional_dimensions <-  NULL
  } else{
    additional_dimensions <- dim_item_sets %>% 
      filter(model_sets %in% disaggs) %>% 
      select(dim_item_uid, dim_uid) %>%
      unique() %>%
      na.omit()
    }
  
  GetDensityDenom(country_uid, de_group, planning_level, technical_area, num_or_den,
                  dissagg_type, period, mechanisms_uid, additional_dimensions, additional_filters = NULL,
                  base_url) 
  Aggregate()
  
  GetDensityNum()
  
  Aggregate()
  MechsHistoricToFuture() # I think I do this for the numerator
  
  CombineNumAndDen()
  

  
  return()
#  sample output?
  SiteToolDensityMatrix <- tibble::tribble(~indicatorCode, ~Age, ~Sex, ~KeyPop, 
                                           ~mechanismCode, ~DsdOrTa, ~OrgUnitUid, 
                                           ~OrgUnitName, ~psnuid,  ~CommOrFac, 
                                           ~value,
                                           "HTS_INDEX_COM.N.Age/Sex/Result.20T.NewNeg", 
                                           "10-14", "Female", NA_character_, "18628", "DSD", "uid11111111", "An Org Unit", "uid22222222","Community", 11)
} 


GetData_Analytics <-  function(base_url, dimensions, filters = NULL){
  
  
  
  api_call <- paste0(base_url,  
                     "api/29/analytics.json?filter=dx:Qdn0vmNSflO;mfYq3HGUIX3", # PMTCT_STAT (N, DSD, Age/Sex/KnownNewResult) TARGET: Known Results
                     # PMTCT_STAT (N, TA, Age/Sex/KnownNewResult) TARGET: Known Results
                     "&dimension=e485zBiR7vG:tIZRQs0FK5P;QOawCj9oLNS", # some age dimension items
                     "&dimension=jyUTj5YC3OK:hDBPKTjUPDm;ZOYVg7Hosni;Gxcf2DK8vNc", # some sex dimension items
                     "&dimension=SH885jaRe0o:", paste0(relevant_mechs, collapse = ";"), # relevant nigeria mechanisms
                     "&dimension=pe:2018Oct", # FY2019 targets perios
                     "&dimension=ou:LEVEL-4;PqlFzhuPcF1", # Nigeria
                     "&outputIdScheme=UID") # gives us UIDs in response                  
  response <- api_call %>% 
    utils::URLencode()  %>%
    RetryAPI("application/json", 20)
  
  content <- response %>% 
    httr::content(., "text") %>% 
    jsonlite::fromJSON()
  
  my_data <- content$rows
  colnames(my_data) <- content$headers$column
  my_data <-tibble::as_tibble(my_data) %>% mutate(Value = as.numeric(Value))
  