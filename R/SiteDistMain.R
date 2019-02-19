
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
  
  dimensions_sample <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                                       "filter", "DE_GROUP-zhdJiWlPvCz","dx", 
                                       "filter", "2018Oct", "pe",
                                       "dimension", "h11OyvlPxpJ", "ou", 
                                       "dimension", "LEVEL-5", "ou", 
                                       "dimension", "f5IPTM7mieH", "LxhLO68FcXm", #tech area = hts_tst
                                       "dimension", "Som9NRMQqV7","lD2x0c8kywj", #numerator or denominator = numerator
                                       "dimension", "iPfNX6Ylqp1","HWPJnUTMjEq",  #disagg type = emergenvy ward...
                                       "dimension", "XU54qYp7mcX", "SH885jaRe0o", 
                                       "dimension", "UQ6CuhPeQvt","SH885jaRe0o", 
                                       "dimension", "b2CX6dbLHo4", "SH885jaRe0o",
                                       "dimension", "egW0hBcZeD2",	"e485zBiR7vG",
                                       "dimension", "Zfg3cHN5TMz",	"e485zBiR7vG",
                                       "dimension", "Gxcf2DK8vNc",	"jyUTj5YC3OK",
                                       "dimension", "Gb0GYkqotaO",	"FokGv0LCTYj")
  
  mechanisms_19T <- datapackcommons::Get19TMechanisms(base_url)
  mechanisms_historic_global <- mechanisms_19T #temp
  country_name <-  "Rwanda"
  de_group <- "zhdJiWlPvCz"
  period <- "2018Oct"
  dimensions_main = c(technical_area = "LxhLO68FcXm",
                      num_or_denom = "Som9NRMQqV7",
                      disagg_type = "HWPJnUTMjEq",
                      ou = "ou",
                      mechanisms = "SH885jaRe0o")
  filters_main <- c(dx = "DE_GROUP-zhdJiWlPvCz",
                    pe = "2018Oct")

  
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
  assertthat::assert_that(NROW(data_element_map) == 1,
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

  