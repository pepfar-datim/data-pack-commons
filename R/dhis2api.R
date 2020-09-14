#' @export
DHISLogin_Play<-function(version) {
  url <- URLencode(URL = paste0("https://play.dhis2.org/", version, "/api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate("admin", "district"),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    r$url %>% 
      stringr::str_remove("api/me")
  }
}

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
RetryAPI <- function(api_url, content_type, max_attempts = 3, timeout = 180){
  for(i in 1:max_attempts){
    try({
      response <- httr::GET(api_url, httr::timeout(timeout))
      if (response$status_code == 200L &&
          response$url == api_url &&
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
#' @title GetCountryLevels(country_names)
#' @description Gets country uid and prioritization level using dataSetAssignments/ous
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param countries_req list of country names to include in response
#' @return dataframe with columns country_level, prioritization_level, country_name, id   

GetCountryLevels <- function(base_url, countries_req = NULL){
  response <-  paste0(base_url, "api/dataStore/dataSetAssignments/orgUnitLevels") %>%
    RetryAPI("application/json", 20)
  
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
                        fields = "name,id")
  
  level_4_countries <- datimutils::getMetadata(organisationUnits,
                          name %.in% countries$country_name,
                          level %.in% "4",
                          fields = "name,id")
  
  assertthat::assert_that(NROW(level_3_countries) + NROW(level_4_countries) == NROW(countries))

# stack level 3 and l;evel 4 countries and join the uid to the main list of countries  
  rbind(level_3_countries, level_4_countries) %>% 
    dplyr::left_join(countries, ., by = c("country_name" = "name")) %>% 
    dplyr::select(-country_name_url)
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
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  dplyr::all_equal response for exact = true or tibble of mismatches if exact = false
#'
ValidateNameIdPairs <- function(names, ids, type, exact = TRUE, base_url = getOption("baseurl")){
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
                                           base_url = base_url)
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
  response <- datimutils::getMetadata(!!type,
                                           filters = id %.in% ids_csv,
                                           fields = "id,code",
                                           base_url = base_url)
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
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return dataframe with the results of the sql view

GetSqlView <- function(sql_view_uid, variable_keys = NULL, variable_values = NULL, 
                       col_types = readr::cols(.default = "c"),
                       base_url = getOption("baseurl")){
  assertthat::assert_that(length(variable_keys) == length(variable_values))
  
  variable_k_v_pairs <- NULL
# format sql variable key value pairs for api call  
  if(length(variable_keys) > 0){
    variable_k_v_pairs <- 
      purrr::map2_chr(variable_keys, variable_values, 
                      ~paste0("var=", .x, ":", .y)) %>% 
      glue::glue_collapse(sep = "&") %>% paste0("?", .)
  }
  
  api_call <- paste0(base_url, "api/sqlViews/", sql_view_uid, "/data.csv", 
                     variable_k_v_pairs) 
  
  RetryAPI(api_call, "application/csv", max_attempts = 1, timeout = 600) %>% 
    httr::content(., "text") %>% 
    readr::read_csv(col_names = TRUE, col_types = col_types)
}

#' @export
#' @title GetData_DataPack
#' @param parameters paramemters for calling an indicator 
#' from datapackcommons::data_required
#' @param  country uid
#' @param include_military Should be TRUE if country has a military org unit,
#' or FALSE if no military org_unit (FALSE for Philippines in COP20)
#' @param dim_item_sets datapackcommons::dim_item_sets or a subset
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return  A list with $time = time the function was called, 
#' $api_call = api call used, and 
#' $results = the data returnd by the analytics call
GetData_DataPack <- function(parameters, 
                             org_units,
                             include_military,
                             dim_item_sets = datapackcommons::dim_item_sets,
                             base_url = getOption("baseurl")) {
  
#  assertthat::assert_that(assertthat::is.string(indicator), nchar(indicator) == 11,
  #                        assertthat::is.string(periods))
  
  assertthat::assert_that(NROW(parameters) == 1)
  
disaggs <- dim_item_sets %>% 
  dplyr::filter(model_sets %in% c(parameters$age_set,
                                  parameters$sex_set,
                                  parameters$kp_set,
                                  parameters$other_disagg_set)) %>%
  dplyr::select(dim_item_uid, dim_uid) %>%
  unique()  %>%
  stats::na.omit() %>%  # there are some items in dim item sets with no source dimension
  dplyr::group_by(dim_uid) %>% 
  dplyr::summarise(dim_item_uids = list(dim_item_uid)) %>% 
  dplyr::ungroup()

disaggs <-   purrr::map2_chr(disaggs$dim_uid, .y = disaggs$dim_item_uids,
                  ~ (!!.x) %.d% .y)
 
types_of_org_units <- datimutils::getDimensions("mINJi7rR1a6",
                                                fields = "items[name,id]",
                                                base_url = base_url,
                                                retry = 5)

  non_mil_types_of_org_units <- dplyr::filter(types_of_org_units, name != "Military") %>% 
    .[["id"]]
  
  mil_type_of_org_units <- dplyr::filter(types_of_org_units, name == "Military") %>% 
    .[["id"]]

  results_psnu <- datimutils::getAnalytics(dx = parameters$dx_id[[1]],
                             pe = parameters$pe_iso[[1]],
                             ou = c(org_units, "OU_GROUP-AVy8gJXym2D"),
                             "mINJi7rR1a6" %.f% non_mil_types_of_org_units,
                             disaggs,
                             retry = 10)
  
  results_mil <- NULL
  if(include_military){    
    results_mil <- 
      datimutils::getAnalytics(dx = parameters$dx_id[[1]],
                               pe = parameters$pe_iso[[1]],
                               ou = c(org_units, "OU_GROUP-nwQbMeALRjL"),
                               disaggs,
                               retry = 5)
  }                                       

  if(is.null(results_psnu) && is.null(results_mil)){ # nothing to return
    results <- NULL
  } else if (is.null(results_mil)) { # psnu but no mil data
    results <- results_psnu
  } else {
    results <- dplyr::bind_rows(results_psnu, results_mil)
  }
  
  return(list("api_call" = NULL,
              "time" = lubridate::now("UTC"),
              "results" = results))
  }