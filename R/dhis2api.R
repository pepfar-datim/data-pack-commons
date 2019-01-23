# devtools::check("/Users/sam/Documents/GitHub/data-pack-commons")
# model_data_pack_input_#######

#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return A parsed list of the configuration file. 
#'
LoadConfigFile <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at",config_path))
    }

    dhis_config <- jsonlite::fromJSON(config_path)
    options("baseurl" = dhis_config$dhis$baseurl)
    options("config" = config_path)
    return(dhis_config)
    } else {
      stop("You must specify a credentials file!") }
}

#' @export
DHISLogin<-function(config_path = NA) {
  
  dhis_config<-LoadConfigFile(config_path)
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate(dhis_config$dhis$username, dhis_config$dhis$password),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

#' @title RetryAPI(api_url, content_type, max_attempts)
#' 
#' @description Submits specified api request up to specified maximum times
#' stopping when expected content type is returned with 200 response
#' @param api_url string - full url for web request
#' @param content_type string - expected type of content in reposne e.d 'application/json'
#' @param max_attempts integer - maximum number of retries for succesful request
#' @return  full api response
#'
RetryAPI <- function(api_url, content_type, max_attempts = 10){
  for(i in 1:max_attempts){
    response <- httr::GET(api_url)
    if (response$status_code == 200L && httr::http_type(response) == content_type){
      return(response)
    }
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response for:", api_url))
}

#' @export
#' @title GetDataWithIndicator(indicator, org_units, level,
#' period, additional_dimensions, additional_filters)
#' 
#' @description Gets data from DHIS2 using a single indicator
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param indicator string - uid of indicator
#' @param org_units list of strings - list of org unit uids, operate as boundry org units
#' @param level string - org hierarchy level, only one allowed
#' @param periods list of strings - periods specified as required by DHIS2 analytics endpoint, 
#' currently only one period supported
#' @param additional_dimensions 2 column dataframe, first column containing dimension item uids
#' and second column the related dimension uid. Dimensions appear as columns in output with names
#' based on dimension name in DHIS2.
#' @param additional_filters 2 column dataframe, first column containing dimension item uids
#' and second column the related dimension uid.filters do not appear explicitly in output.
#' @return  A list with $time = time the function was called, 
#' $api_call = api call used, and 
#' $results = the data returnd by the analytics call
#'

GetDataWithIndicator <- function(base_url, indicator, org_units, level, periods,
                                 additional_dimensions = NULL, additional_filters = NULL) {
  
  assertthat::assert_that(assertthat::is.string(indicator), nchar(indicator) == 11,
                          assertthat::is.string(periods))
  
  org_units <- glue::glue_collapse(org_units, ";")
  
  # prep additional_dimensions for api call
  
  if (!is.null(additional_dimensions)) {
    assertthat::assert_that(NCOL(additional_dimensions) == 2)
    colnames(additional_dimensions)[1] <- "item"
    colnames(additional_dimensions)[2] <- "dimension"
    
    additional_dimensions <-
      additional_dimensions %>% dplyr::group_by(dimension) %>%
      dplyr::summarize(items = paste0(item, collapse = ";")) %>%
      dplyr::mutate(dimension_full = paste0("&dimension=", dimension, ":", items)) %>%
      .[["dimension_full"]] %>% glue::glue_collapse()
  }
  
  # prep additional_filters for api call
  if (!is.null(additional_filters)) {
    assertthat::assert_that(NCOL(additional_filters) == 2)
    colnames(additional_filters)[1] <- "item"
    colnames(additional_filters)[2] <- "filter"
    
    additional_filters <-
      additional_filters %>% dplyr::group_by(filter) %>%
      dplyr::summarize(items = paste0(item, collapse = ";")) %>%
      dplyr::mutate(filter_full = paste0("&filter=", filter, ":", items)) %>%
      .[["filter_full"]] %>% glue::glue_collapse()
  }
  
  web_api_call <- paste0(
    base_url,
    "api/29/analytics.csv?outputIdScheme=UID",
    "&dimension=dx:", indicator,
    "&dimension=pe:", periods,
    "&dimension=ou:LEVEL-", level, ";", org_units,
    additional_dimensions, additional_filters
  )
  
  response <- web_api_call %>% 
    utils::URLencode()  %>%
    RetryAPI("application/csv", 20)

    my_data <- response %>% 
      httr::content(., "text") %>% 
      readr::read_csv(col_names = TRUE, col_types = readr::cols(.default = "c", Value = "d"))
    
    assertthat::has_name(my_data, "Value")
    if(NROW(my_data) > 0){
      assertthat::assert_that(indicator %in% my_data$Data)
    }
    
    #if ("Value" %in% names(my_data)) { # make sure we got a data table - we should always get one back even if empty
      # if we got back an empty data set return it, 
      # if we got back a set with data make sure it the indicator uid matches to validate we got back the data we requested
  #    if(NROW(my_data) == 0 | (NROW(my_data) > 0 & indicator %in% my_data$Data)){
      return(list("api_call" = web_api_call,
                  "time" = lubridate::now("UTC"),
                  "results" = my_data))
 #       }
      #}
#  stop("Call to GetDataWithIndicator failed")
}

#' @export
#' @title GetCountryLevels(country_names)
#' @description Gets country uid and prioritization level using dataSetAssignments/ous
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param countries_req list of country names to include in response
#' @return dataframe with columns country_level, prioritization_level, country_name, id   

GetCountryLevels <- function(base_url, countries_req = NULL){
  response <-  paste0(base_url, "api/dataStore/dataSetAssignments/ous") %>%
    RetryAPI("application/json", 20)
  
# get api response into a data frame a reduce to columns of interest  
  countries <- response %>% httr::content(.,"text") %>%
    jsonlite::fromJSON() %>%
    do.call(rbind.data.frame,.) %>%
    dplyr::mutate(country_name = rownames(.), planning_level = planning, prioritization_level = prioritization, country_level = country) %>% 
    dplyr::select(country_level, planning_level, prioritization_level, country_name) 

# If specific counties were requested filter and assert we got the correct results  
  if(!is.null(countries_req)){
    countries <- countries %>% dplyr::filter(country_name %in% countries_req)
    assertthat::assert_that(NROW(countries) == NROW(countries_req),
                            setequal(countries$country_name, countries_req), 
                            msg = "Error in GetCountryPrioritizationLevel, results do not correspond to countries requested")
  }

# look up country uid by name
# specifing country level as there are some cases of a countries name appearing as an org unit more than once, e.g. Ghana
# countries can appear on level 3 or 4 or org hierarchy currently
  
  assertthat::assert_that(min(countries$country_level) > 2, max(countries$country_level) < 5)
  countries$country_name_url = plyr::laply(countries$country_name, utils::URLencode, reserved = TRUE)

  level_3_countries <- countries %>% dplyr::filter(country_level == "3") %>% .$country_name %>%
    plyr::laply(utils::URLencode, reserved = TRUE) %>% 
    paste0(collapse = ",") %>% 
    paste0("name:in:[", .,"]") %>%  c("level:eq:3") %>% 
    getMetadata(base_url, "organisationUnits", .)
  
  level_4_countries <- countries %>% dplyr::filter(country_level == "4") %>% .$country_name %>%
    plyr::laply(utils::URLencode, reserved = TRUE) %>% 
    paste0(collapse = ",") %>% 
    paste0("name:in:[", .,"]") %>%  c("level:eq:4") %>% 
    getMetadata(base_url, "organisationUnits", .)
  
  assertthat::assert_that(NROW(level_3_countries) + NROW(level_4_countries) == NROW(countries))

# stack level 3 and l;evel 4 countries and join the uid to the main list of countries  
  rbind(level_3_countries, level_4_countries) %>% 
    dplyr::left_join(countries, ., by = c("country_name" = "displayName")) %>% 
    dplyr::select(-country_name_url)
}

#' @export
#' @title getMetadata(base_url, end_point, filters, fields)
#' 
#' @description General utility to get metadata details from DATIM
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param end_point string - api endpoint for the metadata of interest e.g. dataElements, 
#' organisationUnits
#' @param filters - list of strings - the parameters for  the DHIS2 metadata filter, 
#' e.g. c("id:eq:1234","name:in:Kenya,Rwanda")
#' @param fields - string for the fields to return structured as DHIS 2 expects,
#' e.g. "name,id,items[name,id]"
#' @return list of metadata details
getMetadata <- function(base_url, end_point, filters = NULL, fields = NULL) {
  
  url_filters=""
  url_fields=""
  
  if (!is.null(filters)) {
    url_filters <- filters %>% paste0("&filter=", ., collapse = "") %>% URLencode()
  }
  
  if (!is.null(fields)) {
    url_fields <- paste0("&fields=", paste(fields,sep="",collapse=",")) %>% URLencode()
  }
  
  web_api_call <- paste0(base_url, "api/", end_point, ".json?paging=false",
                         url_filters,
                         url_fields)
    r <- web_api_call %>% RetryAPI("application/json", 20)
    # httr::GET()
    assertthat::are_equal(r$status_code, 200L)
#    if (r$status_code == 200L) {
    httr::content(r, "text")   %>%
    jsonlite::fromJSON() %>%
    rlist::list.extract(.,end_point) #} else {
    #  stop("Could not retreive endpoint")
    #}
}

#' @export
#' @title ValidateNameIdPairs(names, ids, type)
#' 
#' @description Checks name list and paired id list (same length) and verifies they correspond to each other
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param names string vector - names of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @return  dplyr::all_equal response
#'
ValidateNameIdPairs <- function(base_url, names, ids, type){
  assertthat::assert_that(is.character(names), assertthat::not_empty(names), NCOL(names) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(names) == length(ids))
  original <- tibble::tibble(name = names, id = ids) %>% unique()
  ids_csv  <-  unique(ids) %>% paste0(collapse = ",")
  response <- datapackcommons::getMetadata(base_url, type, filters = glue::glue("id:in:[{ids_csv}]"), fields = "id,name")
  assertthat::has_name(response, "name")
  assertthat::has_name(response, "id")
  result = dplyr::all_equal(original, response)
  if(result != TRUE){
    stop(list(result=result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
  } else{
    TRUE
  }
}

#' @export
#' @title ValidateCodeIdPairs(base_url, codes, ids, type)
#' 
#' @description Checks code list and paired id list (same length) and verifies they correspond to each other
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param codes string vector - code of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @return  dplyr::all_equal response
#'
ValidateCodeIdPairs <- function(base_url, codes, ids, type){
  assertthat::assert_that(is.character(codes), assertthat::not_empty(codes), NCOL(codes) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(codes) == length(ids))
  original <- tibble::tibble(code = codes, id = ids) %>% unique()
  ids_csv <-  ids %>% unique() %>% paste0(collapse = ",")
  response <- datapackcommons::getMetadata(base_url, type, filters = glue::glue("id:in:[{ids_csv}]"), fields = "id,code")
  assertthat::has_name(response, "code")
  assertthat::has_name(response, "id")
  result <-  dplyr::all_equal(original, response)
  if(result != TRUE){
    stop(list(result=result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
    } else{
    TRUE
    }
  }

# ## No Longer needed function that would add a column with names corresponding to a given
# ## column with UIDs
# add_name_col <- function(data_tib, column_str, end_point_str) {
#   unique_items =   unique(data_tib[[column_str]])
#   unique_items = paste0(unique_items, collapse = ",")
#   item_name_id <-
#     paste0(
#       getOption("baseurl"),
#       "api/",
#       end_point_str,
#       ".csv?paging=false&fields=name,id&filter=id:in:[",
#       unique_items,
#       "]"
#     ) %>%
#     httr::GET() %>%
#     httr::content(., "text")  %>%
#     readr::read_csv(col_names = TRUE)
# 
# 
#   item_name_id <-
#     item_name_id %>%
#     dplyr::rename(!!paste0(column_str, "_name") := name) %>%
#     dplyr::rename(!!column_str := id)
# 
#   data_tib <- dplyr::inner_join(data_tib, item_name_id)
#   return(data_tib)
# }


# VERSION USING THE 5 MAIN DIMESNIONS TO GET DATA - BEFORE SWITCHING TO USE INDICATORS
#' #' export
#' #' param technical_area type string - technical area data element group name
#' #' param numerator_or_denominator type string - numerator / denominator data element group name
#' #' param disagg_to_group_set type string - numerator / denominator data element group name
#' #' disagg_element_to_dimension
#' #' param disagg_element_to_dimension type tibble cols(
#' #' disaggregation_type_element = col_character(),
#' #' data_dimension = col_character()
#' #' ) 
#' #' 
#' GetAnalyticsByDimensions <-
#'   function(technical_area, numerator_or_denominator, disaggregation_type,
#'            targets_or_results, organisation_unit_ids, period,
#'            additional_dimensions = NULL, support_types = c("TA", "DSD")) {
#'     
#'     # Get uids for 5 Standard dimensions     
#'     filters = paste0("name:in:[", technical_area, ",", numerator_or_denominator, ",",
#'                      disaggregation_type, ",", targets_or_results, ",", 
#'                      support_types %>% paste0(collapse = ","),
#'                      "]")
#'     name_uid <- datapackcommons::getMetadata(end_point = "dataElementGroups", filters, fields ="name,id")
#'     
#'     technical_area_uid  <- name_uid %>% dplyr::filter(name==technical_area) %>% .$id
#'     numerator_or_denominator_uid  <- name_uid %>% dplyr::filter(name==numerator_or_denominator) %>% .$id
#'     disaggregation_type_uid  <- name_uid %>% dplyr::filter(name==disaggregation_type) %>% .$id
#'     targets_or_results_uid  <- name_uid %>% dplyr::filter(name==targets_or_results) %>% .$id
#'     support_types_uid <- name_uid %>% dplyr::filter(name %in% support_types) %>% .$id %>% paste0(collapse = ",")
#'     ####
#'     
#'     if (!is.null(additional_dimensions)) {
#'       end_point = "dimensions"
#'       filters = paste0("name:in:[", additional_dimensions %>% paste0(collapse = ","), "]")
#'       additional_dimensions_uid <-
#'         datapackcommons::getMetadata(end_point, filters, "id")$id
#'       additional_dimensions <-
#'         additional_dimensions_uid %>% paste0("&dimension=", ., collapse = "")
#'     }
#'     
#'     # divide org units in to multiple calls
#'     # TODO decide if 40 is reasonable or maybe auto try with smaller chunks if api call fails
#'    
#'     org_unit_chunks <-
#'       organisation_unit_ids %>% split(., ceiling(seq_along(.) / 40))
#'     
#'     for (ou_chunk in 1:NROW(org_unit_chunks))
#'     {
#'       #TODO add error handling
#'       web_api_call <- paste0(getOption("baseurl"), "api/analytics.csv?",
#'                              "dimension=ou:", paste0(org_unit_chunks[[ou_chunk]], collapse = ";"),
#'                              additional_dimensions,
#'                              "&filter=LxhLO68FcXm:", technical_area_uid,
#'                              "&filter=lD2x0c8kywj:", numerator_or_denominator_uid,
#'                              "&filter=TWXpUVE2MqL:", paste0(support_types_uid, collapse = ";"),
#'                              "&filter=HWPJnUTMjEq:", disaggregation_type_uid,
#'                              "&filter=IeMmjHyBUpi:", targets_or_results_uid,
#'                              "&filter=pe:", period
#'       )
#'       data_chunk <- web_api_call  %>%
#'         httr::GET() %>%
#'         httr::content(., "text")
#'       
#'       if (exists("my_data")) {
#'         my_data <- data_chunk %>%
#'           readr::read_csv(col_names = TRUE) %>%
#'           dplyr::bind_rows(my_data, .)
#'       } else{
#'         my_data <- data_chunk %>%
#'           readr::read_csv(col_names = TRUE)
#'       }
#'     }
#'     return(my_data)
#'   }
#' 

## EARLIER VERSION OF GETTING DATA WITH THE % MAIN DIMENSIONS

#' export
# GetAnalytics <-
#   function(technical_area, numerator_or_denominator, support_types,
#            disaggregation_type, targets_or_results,organisation_units,
#            dimensions_row=NULL,dimensions_col=NULL,period){
#     
#     if (!is.null(dimensions_row)) {
#       dimensions_row <-  dimensions_row %>% paste0("&dimension=", ., collapse = "")
#     }
#     
#     if (!is.null(dimensions_col)) {
#       dimensions_col <- dimensions_col %>% paste0("&dimension=", ., collapse = "")
#     }
#     
#     
#     #TODO add error handling
#     web_api_call <- paste0(
#       getOption("baseurl"),
#       "api/analytics.csv?",
#       "dimension=ou:",  paste0(organisation_units,collapse = ";"),
#       dimensions_row, dimensions_col,
#       "&filter=LxhLO68FcXm:", technical_area,
#       "&filter=lD2x0c8kywj:", numerator_or_denominator,
#       "&filter=TWXpUVE2MqL:", paste0(support_types,collapse = ";"),
#       "&filter=HWPJnUTMjEq:", disaggregation_type,
#       "&filter=IeMmjHyBUpi:", targets_or_results,
#       "&filter=pe:", period
#     )
#     #print(web_api_call)
#     
#     data <- web_api_call  %>%
#       httr::GET() %>%
#       httr::content(., "text")
#     
#     data <- data  %>%
#       readr::read_csv(col_names=TRUE)
#     return(data)
#   }

# VERSION OF GETTING DATA WIHT INDICATORS THAT USED NAMES AND DID NOT US LEVEL WHEN CALLING ANALYTICS ENDPOINT
#' 
#' #' export
#' #' param indicator_uid type string - uid of indicator to call
#' #' param organisation_units_uid type character vector -  org unit uids to include in api call
#' #' param period type string - the period to use in api call
#' #' param dimensions_required type list - with dimension names and 
#' #' vector of specific dimension item names for api call e.g.
#' #' list("dimension1" = c("item1"), "dimension1" = c("item1", "item2"), "dimension1" = NULL)
#' #' NULL in the item vector indicates to return all items that are defined to be a member of 
#' #' the dimension in DATIM
#' GetIndicatorByDimensions <- function(indicator_uid, organisation_units_uid, 
#'                                      period, dimensions_required = NULL) {
#'   web_api_call <- paste0(getOption("baseurl"), "api/29/analytics.csv?dimension=dx:", indicator_uid, 
#'                          "&dimension=pe:", period, "&outputIdScheme=NAME")
#'   
#'   # get name,id,items[id,name] for each dimension
#'   end_point = "dimensions"
#'   filters = paste0("name:in:[",
#'                    names(dimensions_required) %>% paste0(collapse = ","),
#'                    "]")
#'   dimensions_metadata <-
#'     datapackcommons::getMetadata(end_point, filters, "name,id,items[id,name]")
#'   
#'   # add each dimension along with any specified items to the api call string    
#'   for (dimension_required in names(dimensions_required)) {
#'     dimension_metadata <-
#'       dimensions_metadata %>% dplyr::filter(name == dimension_required)
#'     if (NROW(dimension_metadata) != 1) {
#'       stop(
#'         paste(
#'           "Unable to match exactly input dimension name to DATIM dimension",
#'           dimension_required
#'         )
#'       )
#'     }
#'     
#'     if (is.null(dimensions_required[[dimension_required]])) {
#'       # There dimension specified without specifc list of items - will return all options
#'       web_api_call <-
#'         paste0(web_api_call, "&dimension=", dimension_metadata$id)
#'       next
#'     } else{
#'       web_api_call <-
#'         #add dimension uid to api call string then contiune to add specific items 
#'         paste0(web_api_call, "&dimension=", dimension_metadata$id, ":")
#'       items_required <-
#'         tibble::tibble(name = dimensions_required[[dimension_required]])
#'     }
#'     
#'     dimension_metadata_items <-
#'       dplyr::left_join(items_required, dimension_metadata[[1, "items"]])
#'     if (anyNA(dimension_metadata_items$id)) {
#'       dimension_metadata_items %>% dplyr::filter(is.na(id)) %>% .$name %>%
#'         paste("Unable to find uid for required dimension item", .) %>% stop()
#'     }
#'     
#'     web_api_call <-  paste0(dimension_metadata_items$id, collapse = ";") %>% paste0(web_api_call, .)
#'   }
#'   
#'   # divide org units in to multiple calls
#'   # TODO decide if 40 is reasonable or maybe auto try with smaller chunks if api call fails
#'   
#'   org_unit_chunks <-
#'     organisation_unit_ids %>% split(., ceiling(seq_along(.) / 40))
#'   
#'   for (ou_chunk in 1:NROW(org_unit_chunks)){
#'     
#'     web_api_call_chunk <- paste0(web_api_call, "&dimension=ou:", 
#'                                  paste0(org_unit_chunks[[ou_chunk]], collapse = ";"))
#'     
#'     data_chunk <- web_api_call_chunk  %>%
#'       httr::GET() %>%
#'       httr::content(., "text")
#'     
#'     if (exists("my_data")) {
#'       my_data <- data_chunk %>%
#'         readr::read_csv(col_names = TRUE) %>%
#'         dplyr::bind_rows(my_data, .)
#'     } else{
#'       my_data <- data_chunk %>%
#'         readr::read_csv(col_names = TRUE)
#'     }
#'   }
#'   return(my_data)
#' }


## EARLY FUNCTION USED TO GET RAW DATA 

#' #' export
#' #' importFrom readr col_character
#' #' importFrom readr col_number
#' #' importFrom readr cols_only
#' #' title GetDataValueSet(org_unit,data_set,start_date,end_date,children)
#' #' param org_unit UID of the organisation unit of interest
#' #' param data_set UID of the dataset of interest
#' #' param start_date Start date in YYYY-MM-DD format
#' #' param end_date End date in YYYY-MM-DD format
#' #' param children Boolean flag to indicate whether data of the Orgunits c
#' #' children should be returned.
#' #' description Gets a data value set for the provided paramaters
#' #' return Only errors out if the file is not readable, does not exist or the
#' #' user cannot login to the server
#' #' 
#' GetDataValueSet <-
#'   function(org_unit,
#'            data_set,
#'            start_date,
#'            end_date,
#'            children = "true") {
#'     #TODO add error handling
#'     web_api_call <- paste0(
#'       getOption("baseurl"),
#'       "api/dataValueSets.csv?dataSet=",
#'       data_set,
#'       "&orgUnit=",
#'       org_unit,
#'       "&children=",
#'       children,
#'       "&startDate=",
#'       start_date,
#'       "&endDate=",
#'       end_date
#'     )
#'     #print(web_api_call)
#'     data <- web_api_call  %>%
#'       httr::GET() %>%
#'       httr::content(., "text")
#'     
#'     data <- data %>% stringr::str_replace(",deleted","") %>%
#'       readr::read_csv(col_names=TRUE, cols_only(dataelement = col_character(),
#'                                                 period = col_character(),
#'                                                 orgunit = col_character(),
#'                                                 categoryoptioncombo = col_character(),
#'                                                 attributeoptioncombo = col_character(),
#'                                                 value = col_number()
#'       ))
#'     return(data)
#'   }
