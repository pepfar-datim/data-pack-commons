
# #' http_was_redirected
# #'
# #' @param response an httr response object, e.g. from a call to httr::GET()
# #'
# #' @return logical of length 1 indicating whether or not any redirect happened 
# #'   during the HTTP request
# #'
# #' @export
# #'
# http_was_redirected <- 
#   function(response){
#     str(response)
#     # extract status 
#     status <- 
#       vapply(
#         X         = response$all_headers, 
#         FUN       = `[[`, 
#         FUN.VALUE = integer(1),
#         "status"
#       )
#     
#     # check status and return
#     any(status >= 300 & status < 400)
#   }

#' @export
#' @title RetryAPI(api_url, content_type, max_attempts)
#' 
#' @description Submits specified api request up to specified maximum times
#' stopping when expected content type is returned with 200 response
#' @param api_url string - full url for web request
#' @param content_type string - expected type of content in reposne e.d 'application/json'
#' @param max_attempts integer - maximum number of retries for succesful request
#' @param timeout integer - maximum time to wait for API response
#' @return  full api response
#'
RetryAPI <- function(api_url, content_type, max_attempts = 3, timeout = 300,
                     d2_session = dynGet("d2_default_session",
                                         inherits = TRUE)){

  for(i in 1:max_attempts){
    try({
      response <- httr::GET(api_url, httr::timeout(timeout), 
                            handle = d2_session$handle)
      if (response$status_code == 200L && 
          stringr::str_remove(response$url, ".*/api/") ==
          stringr::str_remove(api_url,  ".*/api/") &&
          httr::http_type(response) == content_type){
        return(response)
      }
      if (response$status_code >= 300 && 
          response$status_code < 500){ #client error or redirect
        break
      }
      })
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response in RetryAPI for:", api_url))
}

#' @export
#' @importFrom datimutils %.in%
#' @title GetCountryLevels(country_names)
#' @description Gets country uid and prioritization level using dataSetAssignments/ous
#' @param countries_req list of country names to include in response
#' @param d2_session
#' @return dataframe with columns country_level, prioritization_level, country_name, id   

GetCountryLevels <- function(countries_req = NULL,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)){
  response <-  paste0(d2_session$base_url, "api/dataStore/dataSetAssignments/orgUnitLevels") %>%
    RetryAPI("application/json", 20, d2_session = d2_session)
  
# get api response into a data frame a reduce to columns of interest  
  countries <- response %>% httr::content(.,"text") %>%
    jsonlite::fromJSON() %>%
    do.call(rbind.data.frame,.) %>%
    dplyr::mutate(country_name = rownames(.),
                  prioritization_level = prioritization, country_level = country, 
                  community_level = community, facility_level = facility) %>% 
    dplyr::select(country_level, prioritization_level, 
                  facility_level, community_level, country_name) 

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

  level_3_countries <- datimutils::getMetadata(organisationUnits,
                        name %.in% countries$country_name,
                        level %.in% "3",
                        fields = "name,id",
                        d2_session = d2_session)
  
  if (!is.data.frame(level_3_countries)){
    level_3_countries <- NULL
  }
  
  level_4_countries <- datimutils::getMetadata(organisationUnits,
                          name %.in% countries$country_name,
                          level %.in% "4",
                          fields = "name,id",
                          d2_session = d2_session)
  if (!is.data.frame(level_4_countries)){
    level_4_countries <- NULL
  }
  
  assertthat::assert_that(NROW(level_3_countries) + NROW(level_4_countries) == NROW(countries))

# stack level 3 and l;evel 4 countries and join the uid to the main list of countries  
  rbind(level_3_countries, level_4_countries) %>% 
    dplyr::left_join(countries, ., by = c("country_name" = "name")) %>% 
    dplyr::select(-country_name_url)
}

#' @export
#' @title getMetadata(end_point, filters, fields)
#' 
#' @description General utility to get metadata details from DATIM
#' @param end_point string - api endpoint for the metadata of interest e.g. dataElements, 
#' organisationUnits
#' @param filters - list of strings - the parameters for  the DHIS2 metadata filter, 
#' e.g. c("id:eq:1234","name:in:Kenya,Rwanda")
#' @param fields - string for the fields to return structured as DHIS 2 expects,
#' e.g. "name,id,items[name,id]"
#' @param d2_session
#' @return list of metadata details
getMetadata <- function(end_point, 
                        filters = NULL, 
                        fields = NULL,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {
  
  url_filters=""
  url_fields=""
  
  if (!is.null(filters)) {
    url_filters <- filters %>% paste0("&filter=", ., collapse = "") %>% URLencode()
  }
  
  if (!is.null(fields)) {
    url_fields <- paste0("&fields=", paste(fields,sep="",collapse=",")) %>% URLencode()
  }
  
  web_api_call <- paste0(d2_session$base_url, "api/", end_point, ".json?paging=false",
                         url_filters,
                         url_fields)
    r <- web_api_call %>% RetryAPI("application/json", 5,
                                   d2_session = d2_session)
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
#' @param names string vector - names of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @param exact boolean - exact = true matches the full name in datim with the name provided
#' exact = false is a case sensitive serch that the name provided is part of the name in datim
#' @param d2_session
#' @return  dplyr::all_equal response for exact = true or tibble of mismatches if exact = false
#'
ValidateNameIdPairs <- function(names, ids, type, exact = TRUE,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)){
  # TODO the exact and inexact paths were written at different times for different purposes, 
  # harmonize the return format
  assertthat::assert_that(is.character(names), assertthat::not_empty(names), NCOL(names) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(names) == length(ids))
  original <- tibble::tibble(name = names, id = ids) %>% unique()
  ids_csv  <-  unique(ids) %>% paste0(collapse = ",")
  response <- datimutils::getMetadata(!!type,
                                           filters = id %.in% ids_csv,
                                           fields = "id,name",
                                      d2_session = d2_session)
  assertthat::has_name(response, "name")
  assertthat::has_name(response, "id")
  if (exact == TRUE){
    result = dplyr::all_equal(original, response)
    if(result != TRUE){
      stop(list(result=result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
    } 
  } else {
    mismatched  <-  dplyr::left_join(original, response, by = c("id" = "id")) %>%
             dplyr::mutate(match = stringr::str_detect(name.y, name.x)) %>% 
             dplyr::filter(match == FALSE | is.na(match))
    if (NROW(mismatched) > 0){
      return(mismatched %>% 
               dplyr::rename("name" = "name.x", "name_datim"= "name.y"))
    }
    }
  TRUE
  }

#' @export
#' @title ValidateCodeIdPairs
#' 
#' @description Checks code list and paired id list (same length) and verifies they correspond to each other
#' @param codes string vector - code of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @param d2_session
#' @return  dplyr::all_equal response
#'
ValidateCodeIdPairs <- function(codes, ids, type,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)){
  assertthat::assert_that(is.character(codes), assertthat::not_empty(codes), NCOL(codes) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(codes) == length(ids))
  original <- tibble::tibble(code = codes, id = ids) %>% unique()
  ids_csv <-  ids %>% unique() %>% paste0(collapse = ",")
  response <- datimutils::getMetadata(!!type,
                                      filters = id %.in% ids_csv,
                                      fields = "id,code",
                                      d2_session = d2_session)
  assertthat::has_name(response, "code")
  assertthat::has_name(response, "id")
  result <-  dplyr::all_equal(original, response)
  if(result != TRUE){
    stop(list(result=result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
  } else{
    TRUE
    }
}

#' @export
#' @title GetSqlView
#' 
#' @description Runs specified sql view with specified sql view variables and returns
#' a tibble with the results. It is possible to specify the col_types when reading in the data
#' @param sql_view_uid chr - the uid of the sql view
#' @param variable_keys character list - list of the variable names for the sql view
#' @param variable_values character list - list of the variable values ordered to correspond with 
#' the related variable key
#' @param col_types passed to readr::read_csv col_types parameter for parsing the result set
#' @param d2_session
#' @return dataframe with the results of the sql view

GetSqlView <- function(sql_view_uid, variable_keys = NULL, variable_values = NULL, 
                       col_types = readr::cols(.default = "c"),
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)){
  assertthat::assert_that(length(variable_keys) == length(variable_values))
  
  variable_k_v_pairs <- NULL
# format sql variable key value pairs for api call  
  if(length(variable_keys) > 0){
    variable_k_v_pairs <- 
      purrr::map2_chr(variable_keys, variable_values, 
                      ~paste0("var=", .x, ":", .y)) %>% 
      glue::glue_collapse(sep = "&") %>% paste0("?", .)
  }
  
  api_call <- paste0(d2_session$base_url, 
                     "api/sqlViews/", 
                     sql_view_uid, 
                     "/data.csv", 
                     variable_k_v_pairs) 
  
  RetryAPI(api_call, "application/csv", 
           max_attempts = 1, timeout = 600,
           d2_session = d2_session) %>% 
    httr::content(., "text") %>% 
    readr::read_csv(col_names = TRUE, col_types = col_types)
}

#' @export
#' @title GetData_Analytics
#' 
#' @description calls the analytics endpoint using the details in the dimensions parameter
#' dataframe 
#' @param dimensions data frame - must contain columns named "type", "dim_uid", 
#' and "dim_item_uid". Type column contains "filter" or "dimension". dim_uid contains
#' the uid of a dimension or one of the special dimension types e.g. dx, pe, ou, co. 
#' Column dim_item_uid contains the uid of the dimension item to use which can also be
#' a "special" uid such as DE_GROUP-zhdJiWlPvCz  
#' @param d2_session
#' @return data frame with the rows of the response
#'
#' @example
#'  dimensions_sample <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
#' "filter", "vihpFUg2WTy", "dx", #PMTCT positive test rate indicator
#' "dimension", "ImspTQPwCqd", "ou", # sierra leone
#' "dimension", "LEVEL-2", "ou", 
#' "filter", "LAST_YEAR", "pe",
#' "dimension", "UOqJW6HPvvL", "veGzholzPQm",
#' "dimension", "WAl0OCcIYxr", "veGzholzPQm",
#' "dimension", "uYxK4wmcPqA", "J5jldMd8OHv",
#' "dimension", "EYbopBOJWsW", "J5jldMd8OHv")
#' # veGzholzPQm = HIV age, UOqJW6HPvvL = 15-24y, WAl0OCcIYxr = 25-49y, 
#' # J5jldMd8OHv = Facility Type, uYxK4wmcPqA = CHP, EYbopBOJWsW = MCHP
#'   datapackcommons::DHISLogin_Play("2.29")
#'   GetData_Analytics(dimensions_sample, "https://play.dhis2.org/2.29/")

GetData_Analytics <-  function(dimensions,
                               d2_session = dynGet("d2_default_session",
                                                   inherits = TRUE)){
  api_call <- paste0(d2_session$base_url,  
                     "api/29/analytics.json?",
                     datapackcommons::FormatForApi_Dimensions(dimensions, "type",
                                                              "dim_uid", "dim_item_uid"),
                     "&outputIdScheme=UID&hierarchyMeta=true") # gives us UIDs in response                  
  response <- api_call %>% 
    utils::URLencode()  %>%
    RetryAPI("application/json", 3,
             d2_session = d2_session)
  
  content <- response %>% 
    httr::content(., "text") %>% 
    jsonlite::fromJSON()
  
  my_data <- content$rows
  if(length(dim(my_data)) != 2){ # empty table returned
    return(list(results = NULL, 
                api_call = response$url)
           )
  } 
  colnames(my_data) <- content$headers$column
  my_data <- tibble::as_tibble(my_data)

  # list column(vector) of the org hiearchy including the org unit itself
  # added to the data in a mutate below
  ou_hierarchy <- purrr::map_chr(my_data[["Organisation unit"]], 
                                 function(x) paste0(content$metaData$ouHierarchy[[x]], "/", x)) %>% 
    stringr::str_split("/")
  
  my_data <-
    dplyr::mutate(my_data, Value = as.numeric(Value), ou_hierarchy = ou_hierarchy)
  return(list(results = my_data, 
              api_call = response$url)
         )
}


#' @export
#' @title GetData_DataPack
#' @param parameters paramemters for calling an indicator 
#' from datapackcommons::data_required
#' @param  country uid
#' @param include_military Should be TRUE if country has a military org unit,
#' or FALSE if no military org_unit (FALSE for Philippines in COP20)
#' @param dim_item_sets datapackcommons::dim_item_sets or a subset
#' @param d2_session
#' @return  A list with $time = time the function was called, 
#' $api_call = api call used, and 
#' $results = the data returnd by the analytics call
#'
# indicator_parameters <- datapackcommons::StackPrefixedCols(data_required, c("A.", "B.")) %>%
#   unique() %>%
#   filter(!is.na(dx_id))
#  parameters = slice(indicator_parameters, 1)
# # dim_item_sets = datapackcommons::dim_item_sets
#  org_units= "XtxUYCsDWrR"
# # org_unit_levels=NULL
#  GetData_DataPack(parameters=parameters, org_units = org_units)

GetData_DataPack <- function(parameters, 
                             org_units,
                             include_military,
                             dim_item_sets = datapackcommons::dim_item_sets,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
  
#  assertthat::assert_that(assertthat::is.string(indicator), nchar(indicator) == 11,
  #                        assertthat::is.string(periods))
  
  assertthat::assert_that(NROW(parameters) == 1)
  
  
  dimensions <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                                "dimension", parameters$dx_id[[1]], "dx",
                                "dimension", parameters$pe_iso[[1]], "pe")
  
# add rows to dimensions for org units
  if (!is.null(org_units)) {  
    dimensions <- purrr::reduce(org_units, 
                  ~ rbind(.x, 
                          c("dimension", .y, "ou")), 
                  .init = dimensions)
    }    
  
# add rows to dimensions for org unit groups
  # if (!is.null(org_unit_groups)) {
  #   dimensions <- purrr::reduce(org_unit_groups,
  #                 ~ rbind(.x,
  #                         c(
  #                           "dimension", paste0("OU_GROUP-", .y), "ou"
  #                         )),
  #                 .init = dimensions)
  # }
  
  # if (!is.null(org_unit_levels)) {
  #   # add rows to dimensions for levels
  #   dimensions <- purrr::reduce(levels,
  #                 ~ rbind(.x,
  #                         c("dimension", paste0("LEVEL-", .y), "ou")),
  #                 .init = dimensions)
  # }
  

  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
    dplyr::filter(model_sets %in% c(parameters$age_set,
                                    parameters$sex_set,
                                    parameters$kp_set,
                                    parameters$other_disagg_set)) %>% 
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>% 
    stats::na.omit() # there are some items in dim item sets with no source dimension
  
  
  dimensions <- dplyr::bind_rows(dimensions, dimension_disaggs)
  
# Implemented for dreams SNUs for AGYW_PREV   
  if(!is.na(parameters$custom_ou)){
    results_custom <-  
      try({tibble::tibble(type = "dimension",
                          dim_item_uid = parameters$custom_ou,
                          dim_uid = "ou") %>%
          rbind(dimensions) %>% 
          datapackcommons::GetData_Analytics(d2_session = d2_session)}, 
          silent = TRUE) 

    if(is.error(results_custom) || 
       is.null(results_custom[["results"]])){ # nothing to return
      
      api_call <- NULL
      results <- NULL
    } else {
      api_call <- results_custom[["api_call"]]
      results_custom <-  results_custom[["results"]]
      results <- results_custom %>% 
        dplyr::select(-ou_hierarchy)
    }
    return(list("api_call" = api_call,
                "time" = lubridate::now("UTC"),
                "results" = results))
  }

  non_mil_types_of_org_units <- 
    datimutils::getDimensions("mINJi7rR1a6",
                                 fields = "items[name,id]") %>%
    dplyr::filter(name != "Military") %>% 
    .[["id"]]

  
  results_psnu <-  tibble::tibble(type = "filter",
                                  dim_item_uid = non_mil_types_of_org_units,
                                  dim_uid = "mINJi7rR1a6") %>%
    rbind(c("dimension", "OU_GROUP-AVy8gJXym2D", "ou")) %>%  # COP Prioritization SNU) dimensions
    rbind(dimensions) %>% 
    datapackcommons::GetData_Analytics(d2_session = d2_session) 
  
  api_call <- results_psnu[["api_call"]]
  results_psnu <-  results_psnu[["results"]]
  
  results_mil <- NULL
  if(include_military){    
    results_mil <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                                   "dimension", "OU_GROUP-nwQbMeALRjL", "ou") %>%  # military
    rbind(dimensions) %>% 
    datapackcommons::GetData_Analytics() %>% 
    .[["results"]]
}                                       
  
  if(is.null(results_psnu) && is.null(results_mil)){ # nothing to return
    results <- NULL
  } else if (is.null(results_mil)) { # psnu but no mil data
    results <- dplyr::select(results_psnu, -ou_hierarchy)
  } else {
    results <- dplyr::bind_rows(results_psnu, results_mil) %>% 
      dplyr::select(-ou_hierarchy)
  }
  
  
  return(list("api_call" = api_call,
              "time" = lubridate::now("UTC"),
              "results" = results))

  }