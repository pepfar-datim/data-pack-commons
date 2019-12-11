#' @title BuildDimensionList_DataPack(data_element_map_item, dim_item_sets, 
#' country_uid, mechanisms = NULL)
#' 
#' @description get list of dimensions (parameters) for analytics call to get data for SNUxIM 
#' distribution. Tightly coupled to DATIM as it contains some hard coded dimension uids 
#' for Funding Mechanism, technical area, num or denom, disagg type, support type, 
#' and type of organization unit. Also some hard coded dimension items for support type
#' @param data_element_map_item Single row of data_element_map being sliced and passed
#' @param dim_item_sets Dataframe containing all the dimension item sets e.g. datapackcommons::dim_item_sets
#' @param country_uid Country uid
#' @param mechanisms All historic mechanisms for the country filtered by id.
#' When included thee dimensions include psnu, mechanism, AND DSD/TA disaggregation.
#' When null psnu, mechanism and DSD/TA disaggregation are excluded giving country level totals.
#' @return  List of dimensions for the analytics call GetData_Analytics
BuildDimensionList_DataPack <- function(data_element_map_item, dim_item_sets, 
                                        country_uid, mechanisms = NULL){
  
  # prepare df of common dimensions and filters as expected by GetData_analytics  
  dimension_common <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "filter", data_element_map_item[[1,"dx"]],"dx", 
                    "filter", data_element_map_item[[1,"pe"]], "pe",
                    "dimension", country_uid, "ou",
                    "filter", "UwIZeT7Ciz3","sdoDQv2EDjp", # include all mechs without deduplication
                    "dimension", data_element_map_item[[1,"technical_area_uid"]], "LxhLO68FcXm",
                    "dimension", data_element_map_item[[1,"num_or_den_uid"]],"lD2x0c8kywj",
                    "dimension", data_element_map_item[[1,"disagg_type_uid"]],"HWPJnUTMjEq"
    )
  
  # prepare df of dimensions and filters as expected by GetData_analytics  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
    dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>% 
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>% 
    stats::na.omit() # there are some items in dim item sets with no source dimension
  
  if (is.null(mechanisms)){
    return(dplyr::bind_rows(dimension_common, dimension_disaggs))
  }
  
  
  dimension_mechanisms <- mechanisms["mechanism_co_uid"] %>% 
    dplyr::transmute(type = "dimension",
                     dim_item_uid = mechanism_co_uid,
                     dim_uid = "SH885jaRe0o")
  
  # remaining dimensions
  
  tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                  "dimension", "OU_GROUP-nwQbMeALRjL", "ou", # military
                  "dimension", "OU_GROUP-AVy8gJXym2D", "ou", # COP Prioritization SNU
                  "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                  "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
    dplyr::bind_rows(dimension_mechanisms, dimension_disaggs, dimension_common)
}

GetFy20tMechs <- function(base_url = getOption("baseurl")){
  # # SQL view will retrieve list of mechanisms for which there is FY2019 data - 2018Oct period
  # mech_list_parsed <- datapackcommons::GetSqlView("Lh6bMZyyFhd", base_url = base_url)
  # 
  # mech_cat_opt_combos <-  datapackcommons::getMetadata(
  #   base_url, 
  #   "categoryCombos", 
  #   filters = "id:eq:wUpfppgjEza", 
  #   "categoryOptionCombos[code,id~rename(categoryOptionComboId),name,categoryOptions[id~rename(categoryOptionId)]]")[[1,"categoryOptionCombos"]] %>% 
  #   dplyr::as_tibble() %>% 
  #   tidyr::unnest(cols = c(categoryOptions)) %>% 
  #   dplyr::inner_join(mech_list_parsed, by = c("categoryOptionComboId" = "uid"))
  mechs <- datapackcommons::GetSqlView("X6HqWMvcRv0", base_url = base_url)
  if(NROW(mechs) > 0){
    return(mechs)
  }
  # If I got here critical error
  stop("Unable to get 20T mechanisms")
}


devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)

library(datapackcommons)

library(dplyr)
country_name = "Malawi"
DHISLogin("/users/sam/.secrets/jason.json")
base_url <- getOption("baseurl")
# Get the mechanisms relevant for the specifc country being processed
# cache options required for datimvalidation function to work.
# cache age option reverts to original after calling datim validation
country_details = datapackcommons::GetCountryLevels(base_url, country_name)
mechs = GetFy20tMechs() %>% 
  dplyr::filter(country == !!country_name)
# cache_in = getOption("maxCacheAge")
# options(maxCacheAge = 0)
# 
# # if regional country need to use region/operating unit 
# if(country_details$country_level == 3){
#   mechanisms_full_country <- datimvalidation::getMechanismsMap(country_details$id)
# } else if (country_details$country_level == 4) {
#   region_uid <- datapackcommons::getMetadata(base_url, "organisationUnits", 
#                                              filters = paste0("children.id:eq:", country_details$id),
#                                              fields = "id")
#   assertthat::assert_that(NROW(region_uid) == 1)
#   mechanisms_full_country <- datimvalidation::getMechanismsMap(region_uid$id)
# } else{
#   stop("Country level not equal to 3 or 4 in DitributeToSites")
# }
# options(maxCacheAge = cache_in)
# mechanisms_historic_global <- datapackcommons::Get19TMechanisms(getOption("baseurl"))
# # filter to just those mechanisms with data for the relevant time period
# assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, 
#                                              "categoryOptionComboId"),
#                         assertthat::has_name(mechanisms_full_country, "id"))
# mechanisms_historic_country <- mechanisms_historic_global %>%   
#   dplyr::filter(categoryOptionComboId %in% mechanisms_full_country$id)

# alply to call SiteDensity for each row of data_element_map (each target data element)
# will have a historic distribution for each target, DSD/TA, and site given psnu/IM
# alply uses parallel processing here 

getSnuxIm_density <- function(data_element_map_item, 
                              dim_item_sets = datapackcommons::dim_item_sets, 
                              country_uid, planning_level,
                              mechanisms){ 
  
  data <-  BuildDimensionList_DataPack(data_element_map_item, 
                                       dim_item_sets,
                                       country_uid,
                                       mechanisms["mechanism_co_uid"]) %>% 
    datapackcommons::GetData_Analytics() %>% .[["results"]]
  
  if (NROW(data) == 0) return(NULL)

# quick check that data disaggregated by psnu, mechanism, and support type sum to country total    
  checksum <- BuildDimensionList_DataPack(data_element_map_item,
                                          dim_item_sets,
                                          country_uid) %>%
    datapackcommons::GetData_Analytics() %>% .[["results"]] %>% .[["Value"]] %>% sum()

  if(sum(data$Value) != checksum){
    stop(paste("Internal Error: Disaggregated data not summing up to aggregated data in getSnuxIm_density function", sum(data$Value), checksum))
  }
  
  disagg_sets  <-  c("age_set", 
                     "sex_set", 
                     "kp_set", 
                     "other_disagg") %>% 
    purrr::map(~dplyr::filter(dim_item_sets,
                              model_sets == data_element_map_item[[1, .]]))
  
  data <- purrr::reduce(disagg_sets,
                        datapackcommons::MapDimToOptions,
                        allocate = "distribute",
                        .init = data) %>% 
    dplyr::left_join(mechanisms, by = c("Funding Mechanism" = "mechanism_co_uid")) %>% 
    dplyr::mutate(indicator_code = data_element_map_item$indicatorCode_fy20_cop) %>%
    dplyr::rename("value" = "Value",
                  "psnu_uid" = "Organisation unit",
                  "type" = "Support Type") %>% 
  dplyr::select(dplyr::one_of("indicator_code", "psnu_uid",
                                "mechanism_code", "type",
                                "age_option_name", "age_option_uid",
                                "sex_option_name", "sex_option_uid",
                                "kp_option_name", "kp_option_uid",
                                "value"))
  
  if("age_option_name" %in% names(data)){
    data$age_option_name[data$age_option_name == "<1"] <- "<01"
    data$age_option_name[data$age_option_name == "1-4"] <- "01-04"
    data$age_option_name[data$age_option_name == "5-9"] <- "05-09"
    data$age_option_name[data$age_option_name == "<= 2 months"] <- "<= 02 months"
    data$age_option_name[data$age_option_name == "2 - 12 months"] <- "02 - 12 months"
  }
  data$type[data$type == "cRAGKdWIDn4"] <- "TA"
  data$type[data$type == "iM13vdNLWKb"] <- "DSD"
  
  return(data)
}

doMC::registerDoMC(cores = 5)
data = plyr::adply(datapackcommons::Map19Tto20T,
                   1, getSnuxIm_density,
                   datapackcommons::dim_item_sets,
                   country_details$id, country_details$planning_level,
                   GetFy20tMechs() %>% 
                     dplyr::filter(country == !!country_name), 
                   .parallel = TRUE, .expand = FALSE, .id = NULL)

