
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
  
  DHISLogin("/users/sam/.secrets/prod.json")
  base_url <- getOption("baseurl")
  options(maxCacheAge = 0)
  repo_path <- "/users/sam/Documents/GitHub/COP-19-Target-Setting/"
  output_location <- "/Users/sam/COP data/"
  datapack_export = NULL
  
  if(!exists("mechanisms_19T_uid")){
    mechanisms_19T_uid <<- datapackcommons::Get19TMechanisms(base_url)
    }
  country_name <- "Rwanda" 
  
  DistributeToSites(datapack_export, 
                    datapackcommons::Map19Tto20T %>% 
                      filter(stringr::str_detect(technical_area,"TST")), #TODO remove slice
                    mechanisms_19T_uid,
            datapackcommons::dim_item_sets, 
                    datapackcommons::GetCountryLevels(base_url, country_name), base_url)
  
  }

# Takes data pack export and details for distributiong it and returns required output for site tool 
# import process
DistributeToSites <- function(datapack_data, 
                              data_element_map, 
                              mechanisms_historic_global,
                              dim_item_sets,
                              country_details, base_url){
  
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_details$id)
  
  assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, "uid"))
  assertthat::assert_that(assertthat::has_name(mechanisms_full_country, "id"))
  
  mechanisms_historic_country <- mechanisms_historic_global %>% 
    dplyr::filter(uid %in% mechanisms_full_country$id)

  # temp = CalculateSiteDensity(dplyr::slice(data_element_map,1), 
  #                      country_details, mechanisms_historic_country, dim_item_sets)
  # return(temp)
  
# adply to call SiteDensity for each row of data_element_map
# will have a distribution for each DSD/TA, site given psnu/IM
  
  doMC::registerDoMC(cores = 5) # or however many cores you have access to
  site_densities <- plyr::alply(data_element_map, 1, CalculateSiteDensity, 
              country_details, mechanisms_historic_country, dim_item_sets, base_url, .parallel = TRUE)
  # match density with datapack data
  return(site_densities)
   }

# 
CalculateSiteDensity <- function(data_element_map_item, country_details, 
                                 mechanisms, dim_item_sets, base_url){
  assertthat::assert_that(NROW(data_element_map_item) == 1,
                          NROW(country_details) == 1)
  
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

# dimensions used by both numerator and denominator and prepped for GetData_Analytics
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
    if(NROW(density_denominators$results) == 0){
      return("No Data") # to do return something more useful?
    }
    return(density_denominators)
    # Join analytics output (dimensions) to category options
    age_set <- dim_item_sets %>% 
      filter(model_sets == data_element_map_item[[1,"age_set"]])
    sex_set <- dim_item_sets %>% 
      filter(model_sets == data_element_map_item[[1,"sex_set"]])
    kp_set <-  dim_item_sets %>% 
      filter(model_sets == data_element_map_item[[1, "kp_set"]])
    other_disagg <-  dim_item_sets %>% 
      filter(model_sets == data_element_map_item[[1, "other_disagg"]])
    mapped_data <- list(density_denominators$results, 
                        age_set, 
                        sex_set, 
                        kp_set,
                        other_disagg) %>% 
      purrr::reduce(MapDimToOptions, allocate = "distribute")
    
    return(mapped_data)
    # %>% 
    #   RenameAnalyticsColumns()
    
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


### COPY OF FUNCTION CURRENTLY IN COP!9 REPO MODEL CALCULATIONS DO NOT CHANGE
### WE SHOULD INCORPORATE INTO DATA PACK COMMONS PACKAGE
RenameDimensionColumns <- function(data, type){
  data %>% dplyr::rename(!!paste0(type,"_dim_uid") := dim_uid,
                         !!paste0(type,"_dim_name") := dim_name,
                         !!paste0(type,"_dim_cop_type") := dim_cop_type,
                         !!paste0(type,"_dim_item_name") := dim_item_name,
                         !!paste0(type,"_option_name") := option_name,
                         !!paste0(type,"_option_uid") := option_uid,
                         !!paste0(type,"_sort_order") := sort_order,
                         !!paste0(type,"_weight") := weight,
                         !!paste0(type,"_model_sets") := model_sets) %>% return()
}

### COPY OF FUNCTION CURRENTLY IN COP!9 REPO MODEL CALCULATIONS DO NOT CHANGE
### WE SHOULD INCORPORATE INTO DATA PACK COMMONS PACKAGE

MapDimToOptions <- function(data, items_to_options, allocate){
  
  if(NROW(items_to_options) == 0){
    return(data)
  }
  
  dimension_uid <- unique(items_to_options$dim_uid)
  cop_category <- unique(items_to_options$dim_cop_type)
  assertthat::assert_that(NROW(dimension_uid) == 1, NROW(cop_category) == 1)
  
  if(is.na(dimension_uid)){
    # We are in a scenario of distributing to category options in the absence of a source dimension
    # so we need cartesian product of data with item_to_dim entries
    joined_data <- tidyr::crossing(data, items_to_options)
  } else {
    dim_name <-  items_to_options[[1,"dim_name"]]
    joined_data <- data %>%
      left_join(items_to_options, by = setNames("dim_item_uid", dim_name))
  }
  
  if(allocate == "distribute"){
    joined_data %>%
      mutate(Value = Value * weight) %>%
      RenameDimensionColumns(stringr::str_remove(cop_category, "_set"))
  } else{
    joined_data %>%
      RenameDimensionColumns(stringr::str_remove(cop_category, "_set"))
  }
}

