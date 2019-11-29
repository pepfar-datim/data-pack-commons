#' @title BuildDimensionLists(data_element_map_item, dim_item_sets, mechanisms, country_details)
#' 
#' @description # get list of dimensions (parameters) for analytics call by level {planning, community, facility}
#' analytics will be called seperatly for each level.
#' Tightly coupled to DATIM as it contains some hard coded dimension uids for Funding Mechanism, technical area, 
#' num or denom, disagg type, support type, and type of organization unit. Also some hard coded dimension items for
#' type of organization unit and support type
#' @param data_element_map_item Single row of data_element_map being sliced and passed
#' @param dim_item_sets Dataframe containing all the dimension item sets e.g. datapackcommons::dim_item_sets
#' @param mechanisms All historic mechanisms for the country filtered by id
#' @param country_details Country levels for a country
#' @return  List of dimensions for the analytics call GetData_Analytics
BuildDimensionLists_Mechanisms <- function(data_element_map_item, dim_item_sets, 
                                mechanisms, country_details){
  # prepare df of dimensions and filters as expected by GetData_analytics  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
    dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>% 
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>% 
    stats::na.omit() # there are some items in dim item sets with no source dimension
  
  dimension_mechanisms <- mechanisms["categoryOptionId"] %>% dplyr::transmute(type = "dimension", 
                                                                              dim_item_uid = categoryOptionId,
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
  
  dimensions_planning <- #filter on community and facility gets rid of military PSNU
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "dimension", paste0("LEVEL-", country_details$planning_level), "ou") %>% 
    dplyr::bind_rows(dimensions_common)
  
  out <- list(planning = dimensions_planning)

  if(data_element_map_item$facility_valid == TRUE){
    dimensions_facility <- 
      tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                      "dimension", paste0("LEVEL-", country_details$facility_level), "ou",
                      "dimension", "POHZmzofoVx", "mINJi7rR1a6", # facility org type
                      "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                      "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
      dplyr::bind_rows(dimensions_common)
    
    out$planning <- rbind(out$planning, 
                                 c("filter", "POHZmzofoVx", "mINJi7rR1a6"))
    out$facility <-  dimensions_facility
  }
  
  if(data_element_map_item$community_valid == TRUE){
    dimensions_community <- 
      tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                      "dimension", paste0("LEVEL-", country_details$community_level), "ou",
                      "dimension", "PvuaP6YALSA","mINJi7rR1a6", # community org type
                      "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                      "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
      dplyr::bind_rows(dimensions_common)
    
    out$planning <- rbind(out$planning, 
                          c("filter", "PvuaP6YALSA", "mINJi7rR1a6"))
    out$community = dimensions_community
  }
  
return(out)
}


devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)
library(datapackcommons)
library(dplyr)
country_details = datapackcommons::GetCountryLevels(base_url, "Rwanda")
DHISLogin("/users/sam/.secrets/jason.json")
base_url <- getOption("baseurl")
# Get the mechanisms relevant for the specifc country being processed
# cache options required for datimvalidation function to work.
# cache age option reverts to original after calling datim validation
cache_in = getOption("maxCacheAge")
options(maxCacheAge = 0)

# if regional country need to use region/operating unit 
if(country_details$country_level == 3){
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_details$id)
} else if (country_details$country_level == 4) {
  region_uid <- datapackcommons::getMetadata(base_url, "organisationUnits", 
                                             filters = paste0("children.id:eq:", country_details$id),
                                             fields = "id")
  assertthat::assert_that(NROW(region_uid) == 1)
  mechanisms_full_country <- datimvalidation::getMechanismsMap(region_uid$id)
} else{
  stop("Country level not equal to 3 or 4 in DitributeToSites")
}
options(maxCacheAge = cache_in)
mechanisms_historic_global <- datapackcommons::Get19TMechanisms(getOption("baseurl"))
# filter to just those mechanisms with data for the relevant time period
assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, 
                                             "categoryOptionComboId"),
                        assertthat::has_name(mechanisms_full_country, "id"))
mechanisms_historic_country <- mechanisms_historic_global %>%   
  dplyr::filter(categoryOptionComboId %in% mechanisms_full_country$id)

# alply to call SiteDensity for each row of data_element_map (each target data element)
# will have a historic distribution for each target, DSD/TA, and site given psnu/IM
# alply uses parallel processing here 



BuildDimensionLists_Mechanisms(datapackcommons::Map19Tto20T %>% slice(1), 
                               datapackcommons::dim_item_sets,
                               mechanisms_historic_country,
                               datapackcommons::GetCountryLevels(base_url, "Rwanda"))
