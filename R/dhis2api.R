# devtools::check("/Users/sam/Documents/GitHub/data-pack-commons")
# model_data_pack_input_20190201_2

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
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    return(TRUE)
  }
}

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
    try({
      response <- httr::GET(api_url, httr::timeout(180))
      if (response$status_code == 200L && 
          response$url == api_url && 
          httr::http_type(response) == content_type){
        return(response)
        }
      })
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response in RetryAPI for:", api_url))
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
  
#  for(i in 1:3){
#    try({
      response <- web_api_call %>% 
        utils::URLencode()  %>%
        RetryAPI("application/csv", 20)
      
      my_data <- response %>% 
        httr::content(., "text") %>% 
        readr::read_csv(col_names = TRUE, col_types = readr::cols(.default = "c", Value = "d"))
      
      assertthat::has_name(my_data, "Value") 
      # not sure I need preceeding line now that I verify content type in retry api
      # this was here to catch recieving the "log in" screen which was html
      
      if(NROW(my_data) > 0 && !(indicator %in% my_data$Data)){
        stop("response$url: ", response$url, " slice(my_data,1): ", dplyr::slice(my_data,1))
        assertthat::assert_that(indicator %in% my_data$Data)
      }
#      break # if I am here then I got a valid result set
#    })
#    if(i == 3){stop("three attempts to obtain valid result set in GetDataWithIndicator failed")}
#    }
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
    dplyr::mutate(country_name = rownames(.), planning_level = planning, 
                  prioritization_level = prioritization, country_level = country, 
                  community_level = community, facility_level = facility) %>% 
    dplyr::select(country_level, planning_level, prioritization_level, 
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

#' @export
#' @title Get19TMechanisms(base_url)
#' 
#' @description gets data from SQL view vtNAsZcMZiU - mechanisms with 2018Oct data
#' @param base_url string - base address of instance (text before api/ in URL)
Get19TMechanisms <- function(base_url = getOption("baseurl")){
  # SQL view will retrieve list of mechanisms for which there is FY2019 data - 2018Oct period
  api_call <- paste0(base_url, "api/sqlViews/vtNAsZcMZiU/data.csv") 
  # limit calls to this SQL view on prod
  # it locks data value table
  mech_list_r <-  RetryAPI(api_call, "application/csv")
  mech_list_parsed <- mech_list_r %>% 
    httr::content(., "text") %>% 
    readr::read_csv(col_names = TRUE, col_types = readr::cols(.default = "c"))
  
  mech_cat_opt_combos <-  datapackcommons::getMetadata(base_url, 
                                      "categoryCombos", 
                                      filters = "id:eq:wUpfppgjEza", 
                                      "categoryOptionCombos[code,id~rename(categoryOptionComboId),name,categoryOptions[id~rename(categoryOptionId)]]")[[1,"categoryOptionCombos"]] %>% 
    dplyr::as_tibble() %>% 
    tidyr::unnest() %>% 
    dplyr::inner_join(mech_list_parsed, by = c("categoryOptionComboId" = "uid"))
  
  if(NROW(mech_cat_opt_combos) > 0){
    return(mech_cat_opt_combos)
  }
  # If I got here critical error
  stop("Unable to get 19T mechanisms")
}

#' @export
#' @title GetData_Analytics <-  function(dimensions, base_url)
#' 
#' @description calls the analytics endpoint using the details in the dimensions parameter
#' dataframe 
#' @param dimensions data frame - must contain columns named "type", "dim_uid", 
#' and "dim_item_uid". Type column contains "filter" or "dimension". dim_uid contains
#' the uid of a dimension or one of the special dimension types e.g. dx, pe, ou, co. 
#' Column dim_item_uid contains the uid of the dimension item to use which can also be
#' a "special" uid such as DE_GROUP-zhdJiWlPvCz  
#' @param base_url string - base address of instance (text before api/ in URL)
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

GetData_Analytics <-  function(dimensions, base_url = getOption("baseurl")){
  api_call <- paste0(base_url,  
                     "api/29/analytics.json?",
                     datapackcommons::FormatForApi_Dimensions(dimensions, "type", 
                                                              "dim_uid", "dim_item_uid"),
                     "&outputIdScheme=UID&hierarchyMeta=true") # gives us UIDs in response                  
  response <- api_call %>% 
    utils::URLencode()  %>%
    RetryAPI("application/json", 20)
  
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

  ##TODO Sid add some code to validate what we got back from api matches what we requestd
  ## Perhaps make sure there are columns where we specified the actual uid for a dimension and that 
  ##the items in these columns were requested.
  ## If there is a mismatch stop()
  ## is there other useful metadata we want to return?
  
  # Assertion to check output for dim_uid "veGzholzPQm"
  # Taken from the input
  dim_list <- dimensions %>% 
    filter(dimensions$dim_uid == "veGzholzPQm")
  dim_list <- unique(dim_list$dim_item_uid)
  pipe_dim_list <- paste(dim_list, collapse = "|")
  
  # Asserting the values from input being present in output column of api call
  assertthat::assert_that(grepl(pipe_dim_list, my_data[3]) == TRUE)
  
  # Assertion to check output for dim_uid "J5jldMd8OHv"
  # Taken from the input
  facility_list <- dimensions %>% 
    filter(dimensions$dim_uid == "J5jldMd8OHv")
  facility_list <- unique(dim_list$dim_item_uid)
  pipe_facility_list <- paste(dim_list, collapse = "|")
  
  # Asserting the values from input being present in output column of api call
  assertthat::assert_that(grepl(pipe_facility_list, my_data[1]) == TRUE)
  
  # Assertion for detecting the right OU being present
  assertthat::assert_that(all(grepl(dimensions[2,2] ,content$metaData$ouHierarchy)) == TRUE)

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
