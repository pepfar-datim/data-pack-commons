# install.packages("tidyverse")
# library(tidyverse)
# library(httr)
#
# validDPDisaggs <- function() {
#
#   validDisaggs <- list(
#     "Epi Cascade I" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "Epi Cascade II" = list(
#       validAges = c("<15","15+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "Epi PMTCT" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "Prioritization" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "PMTCT_STAT_ART" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female"),
#       validKPs = NA_character_),
#     "PMTCT_EID" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "TB_STAT_ART" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "VMMC" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Male"),
#       validKPs = NA_character_),
#     "TX" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "CXCA" = list(
#       validAges = c("25-29","30-34","35-39","40-44","45-49"),
#       validSexes = c("Female"),
#       validKPs = NA_character_),
#     "HTS" = list(
#       validAges = c("01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "TB_TX_PREV" = list(
#       validAges = c("<15","15+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "OVC" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-17","18+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "KP" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = c("Female PWID","Male PWID","PWID","FSW","MSM not SW","MSM SW","MSM","People in prisons and other enclosed settings","TG SW","TG not SW","TG")),
#     "PP" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "PrEP" = list(
#       validAges = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "GEND" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_)
#   )
#
#   return(validDisaggs)
# }
#
# datimvalidation::loadSecrets(secrets)
#
# config_path = "/Users/siddharth/.secrets/datim.json"
#
# LoadConfig(config_path)
#
# psnu_levels <-
#   paste0(getOption("baseurl"),
#          "api/dataStore/dataSetAssignments/ous") %>%
#   GET() %>%
#   content(., "text") %>%
#   fromJSON(., flatten = TRUE) %>%
#   do.call(rbind.data.frame, .) %>%
#   select(name3, prioritization,community,facility) %>%
#   mutate(name3 = as.character(name3))
#
# disaggs <- validDPDisaggs() %>%
#   magrittr::extract2("TX") %>%
#   purrr::cross_df()
#
# cross_output <- tidyr::crossing(1:18189, disaggs)
#
# write_csv(cross_output, "/Users/siddharth/Desktop/Output.csv")

#Produce Site List and Mech List for use in Site Level Review Tool

#CHANGE ME!
#config_path="~/.secrets/datim.json"
config_path="/Users/siddharth/.secrets/datim.json"
#outFolder="/home/jason/consultancy/datim/datapack/"
outFolder="/Users/siddharth/Desktop/"

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
  if(NROW(my_data) > 0 && !(indicator %in% my_data$Data)){
    stop("response$url: ", response$url, " slice(my_data,1): ", slice(my_data,1))
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

#DON'T CHANGE!

require(dplyr)
require(httr)
require(jsonlite)
require(stringr)

LoadConfig<-function(config_path=NA) {
  #Load from a file
  if (!is.na(config_path) ) {
    foo <- assertthat::assert_that(file.exists(config_path))
    s<-fromJSON(config_path)
    options("baseurl"= s$dhis$baseurl )
    options("config"=config_path)
    url<-URLencode( URL = paste0(getOption("baseurl"),"api/me") )
    #Logging in here will give us a cookie to reuse
    r<-GET(url ,
           authenticate(s$dhis$username,s$dhis$password),
           timeout(60))
    assertthat::assert_that(r$status == 200L)
    print("Successfully logged in!")
    r<- content(r, "text")
    me<-fromJSON(r)
    options("organisationUnit" = me$organisationUnits$id) } else {
      stop("You must specify a credentials file!")
    }
}

PSNU_levels <- GetCountryLevels("https://www.datim.org/")

get_full_site_list <- function(config_path) {

  LoadConfig(config_path)

  psnu_levels <-
    paste0(getOption("baseurl"),
           "api/dataStore/dataSetAssignments/ous") %>%
    GET() %>%
    content(., "text") %>%
    fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    select(name3, prioritization,community,facility) %>%
    mutate(name3 = as.character(name3))


  orgHierarchy <-
    paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
    GET() %>%
    content(., "text") %>%
    fromJSON(., flatten = TRUE)


  ous_list<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
    setNames(.,orgHierarchy$headers$name) %>%
    dplyr::inner_join(psnu_levels,by=c("level3name" = "name3")) %>%
    mutate(level = as.numeric(level)) %>%
    mutate(
      psnu_name = case_when(
        prioritization == 4 ~ level4name,
        prioritization == 5 ~ level5name,
        prioritization == 6 ~ level6name),
      psnu_uid = case_when(
        prioritization == 4 ~ uidlevel4,
        prioritization == 5 ~ uidlevel5,
        prioritization == 6 ~ uidlevel6)
    ) %>%
    mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
    select(organisationunituid,name,level,prioritization,community,facility,uidlevel3,level3name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
    mutate(
      siteType = case_when(
        stringr::str_detect(name, "_Military ") ~ "Military",
        level == facility ~ "Facility",
        level == community  ~ "Community"),
      distributed=1) %>%
    select(distributed,siteType,everything())

  #Add in _Military sites again for cases where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(siteType=="Military") %>%
    mutate(distributed=0) %>%
    bind_rows(ous_list,.)

  #Add in non-clustered PSNUs where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(level==prioritization & !str_detect(name,"_Military")) %>%
    mutate(siteType="PSNU",
           distributed=0) %>%
    bind_rows(ous_list,.) %>%
    #Filter out anything not tagged at this point
    filter(!is.na(siteType)) %>%
    #Construct Site Tool Name
    mutate(DataPackSiteID=case_when(distributed==0 ~ paste0(name," > NOT YET DISTRIBUTED (",organisationunituid,")"),
                                    siteType=="Military" ~ paste0(name," (",organisationunituid,")"),
                                    siteType=="Facility" ~ paste0(dpOrgUnit," > ",name," {Facility} (",organisationunituid,")"),
                                    siteType=="Community" ~ paste0(dpOrgUnit," > ",name," {Community} (",organisationunituid,")"))) %>%
    select(DataPackSiteUID=organisationunituid,DataPackSiteID,ou_uid=uidlevel3,ou_name=level3name,siteType,distributed) %>%
    unique()

  return(ous_list)
}

getMechList <- function(config_path) {

  LoadConfig(config_path)
  url<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv")
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("mechanism","code","uid","ou")])

}

full_site_list <- get_full_site_list(config_path)

fill_mech_list <- getMechList(config_path)

# In case the site list and mech list need to be stored as RDA files on local
# site_list <- get_site_list(config_path) %>%
#   saveRDS(paste0(outFolder,"/ous_list.rda"))

# getMechList(config_path) %>%
#   saveRDS(paste0(outFolder,"/mech_list.rda"))

# full_site_list <- readRDS("/Users/siddharth/Desktop/ous_list.rda")

#full_mech_list <- readRDS("/Users/siddharth/Desktop/mech_list.rda")

colnames(full_site_list)[4] <- "country_name"
print(full_site_list[4])


merged_table <- merge(full_site_list, PSNU_levels, by = "country_name")
# There's a problem here, the merged table has 107k rows and the full site list has 111k rows.

subset_table <- merged_table[c("country_name", "DataPackSiteID", "DataPackSiteUID", "siteType")]
