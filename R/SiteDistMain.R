
main <- function(){
  devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                    build = TRUE,
                    upgrade = FALSE)
  
  require(plyr)
  require(datapackcommons)
  require(tidyverse)
  require(jsonlite)
  # require(lubridate)
  # require(rlang)
  require(assertthat)
  require(foreach)
  # # require(rlist)
  # 
  require(datimvalidation)
  
  DHISLogin("/users/sam/.secrets/triage.json")
  base_url <- getOption("baseurl")
  options(maxCacheAge = 0)
  repo_path <- "/users/sam/Documents/GitHub/COP-19-Target-Setting/"
  output_location <- "/Users/sam/COP data/"
  datapack_export = NULL
  
  # dimensions_sample <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
  #                                      "filter", "DE_GROUP-zhdJiWlPvCz","dx",
  #                                      "filter", "2018Oct", "pe",
  #                                      "dimension", "h11OyvlPxpJ", "ou",
  #                                      "dimension", "LEVEL-5", "ou",
  #                                      "dimension", "f5IPTM7mieH", "LxhLO68FcXm", #tech area = hts_tst
  #                                      "dimension", "Som9NRMQqV7","lD2x0c8kywj", #numerator or denominator = numerator
  #                                      "dimension", "iPfNX6Ylqp1","HWPJnUTMjEq",  #disagg type = emergenvy ward...
  #                                      "dimension", "XU54qYp7mcX", "SH885jaRe0o",
  #                                      "dimension", "UQ6CuhPeQvt","SH885jaRe0o",
  #                                      "dimension", "b2CX6dbLHo4", "SH885jaRe0o",
  #                                      "dimension", "egW0hBcZeD2",	"e485zBiR7vG",
  #                                      "dimension", "Zfg3cHN5TMz",	"e485zBiR7vG",
  #                                      "dimension", "Gxcf2DK8vNc",	"jyUTj5YC3OK",
  #                                      "dimension", "Gb0GYkqotaO",	"FokGv0LCTYj")
  
  mechanisms_19T_uid <- datapackcommons::Get19TMechanisms(base_url)
  country_name <- "Rwanda" 
  datapack_export <- NULL
  DistributeToSites(datapack_export, 
                    datapackcommons::Map19Tto20T %>% filter(stringr::str_detect(technical_area,"PMTCT")), #TODO remove slice
                    mechanisms_19T_uid,
                    datapackcommons::dim_item_sets, 
                    datapackcommons::GetCountryLevels(base_url, country_name))
  
  }

# Takes data pack export and details for distributiong it and returns required output for site tool 
# import process
DistributeToSites <- function(datapack_data, 
                              data_element_map, 
                              mechanisms_historic_global,
                              dim_item_sets,
                              country_details){
  
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_details$id)
  
  assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, "uid"))
  assertthat::assert_that(assertthat::has_name(mechanisms_full_country, "id"))
  
  mechanisms_historic_country <- mechanisms_historic_global %>% 
    filter(uid %in% mechanisms_full_country$id)

  # temp = CalculateSiteDensity(dplyr::slice(data_element_map,1), 
  #                      country_details, mechanisms_historic_country, dim_item_sets)
  # return(temp)
# adply to call SiteDensity for each row of data_element_map
# will have a distribution for each DSD/TA, site given psnu/IM
  doMC::registerDoMC(cores = 5) # or however many cores you have access to
  site_densities <- plyr::alply(data_element_map, 1, CalculateSiteDensity, 
              country_details, mechanisms_historic_country, dim_item_sets, .parallel = TRUE)
  # match density with datapack data
  return(site_densities)
   }

# 
CalculateSiteDensity <- function(data_element_map_item, country_details, 
                                 mechanisms, dim_item_sets){
  assertthat::assert_that(NROW(data_element_map_item) == 1,
                          NROW(country_details) == 1)
  base_url <- getOption("baseurl")
# prepare df of dimensions and filters as expected by GetData_analytics  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
           dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>% 
                     dplyr::select(type, dim_item_uid, dim_uid) %>%
           unique()  %>% 
           na.omit() # there are some items in dim item sets with no source dimension
  
  dimension_mechanisms <- mechanisms %>% dplyr::transmute(type = "dimension", 
                                                       dim_item_uid = uid,
                                                       dim_uid = "SH885jaRe0o")
  
  dimensions_common <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "filter", data_element_map_item[[1,"dx"]],"dx", 
                    "filter", data_element_map_item[[1,"pe"]], "pe",
                    "dimension", country_details$id, "ou",
                    "dimension", data_element_map_item[[1,"technical_area_uid"]], "LxhLO68FcXm",
                    "dimension", data_element_map_item[[1,"num_or_den_uid"]],"lD2x0c8kywj",
                    "dimension", data_element_map_item[[1,"disagg_type_uid"]],"HWPJnUTMjEq") %>% 
    dplyr::bind_rows(dimension_mechanisms, dimension_disaggs)
  
  # denominator adds planning level to dimensions_common ou  
    density_denominators <- 
      tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                      "dimension", paste0("LEVEL-", country_details$planning_level), "ou") %>% 
      dplyr::bind_rows(dimensions_common) %>% 
      datapackcommons::GetData_Analytics(base_url)
    return(density_denominators)
    
### aggregate
    
  #numerator adds dsd/ta 
    stop() ## working stoped here
    
    
    
    density_numerators_fac <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                                            "dimension", paste0("LEVEL-", country_details$facility_level), "ou") %>% 
      dplyr::bind_rows(dimensions_common) %>% 
      datapackcommons::GetData_Analytics(base_url)
  
#  numerator called seperatly for planning facility and military
  
  
  
  GetDensityDenom(country_uid, de_group, planning_level, technical_area, num_or_den,
                  dissagg_type, period, mechanisms_uid, additional_dimensions, additional_filters = NULL,
                  base_url) 
  Aggregate()
  
  GetDensityNum()
  
  Aggregate()
  MechsHistoricToFuture() # I think I do this for the numerator
  
  CombineNumAndDen()
  
#  sample output?
  SiteToolDensityMatrix <- tibble::tribble(~indicatorCode, ~Age, ~Sex, ~KeyPop, 
                                           ~mechanismCode, ~DsdOrTa, ~OrgUnitUid, 
                                           ~OrgUnitName, ~psnuid,  ~CommOrFac, 
                                           ~value,
                                           "HTS_INDEX_COM.N.Age/Sex/Result.20T.NewNeg", 
                                           "10-14", "Female", NA_character_, "18628", "DSD", "uid11111111", "An Org Unit", "uid22222222","Community", 11)
} 

