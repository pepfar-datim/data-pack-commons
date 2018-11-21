

#' @export
#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return Only errors out if the file is not readable, does not exist or the
#' user cannot login to the server
#'
LoadConfig <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = -1)) {
      stop(paste("Cannot read configuration located at",config_path))
    }
    s <- jsonlite::fromJSON(config_path)
    options("baseurl" = s$dhis$baseurl)
    options("config" = config_path)
    url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
    #Logging in here will give us a cookie to reuse
    r <- httr::GET(url ,
             httr::authenticate(s$dhis$username, s$dhis$password),
             httr::timeout(60))
    if(r$status != 200L){
      stop("Could not authenticate you with the server!")
    } else {
      print("Successfully logged in!")
      me <- httr::content(r, as="parsed",type="application/json")
      options("organisationUnit" = me$organisationUnits$id)
    }
  } else {
    stop("You must specify a credentials file!")
  }
}

getMetadata <- function(end_point, filters = NULL, fields = NULL) {
  if (!is.null(filters)) {
    filters <- filters %>% paste0("&filter=", ., collapse = "") %>% URLencode()
  }

  if (!is.null(fields)) {
    fields <- fields %>% paste0("&fields=", .) %>% URLencode()
  }

  web_api_call <- paste0(getOption("baseurl"),
                         "api/",
                         end_point,
                         ".json?paging=false",
                         filters,
                         fields)
  #print(web_api_call)
  data <- web_api_call %>%  httr::GET() %>%
    httr::content(., "text")   %>%
    jsonlite::fromJSON()
  
  return(data[[end_point]])
}

#' @export
#' @importFrom readr col_character
#' @importFrom readr col_number
#' @importFrom readr cols_only
#' @title GetDataValueSet(org_unit,data_set,start_date,end_date,children)
#' @param org_unit UID of the organisation unit of interest
#' @param data_set UID of the dataset of interest
#' @param start_date Start date in YYYY-MM-DD format
#' @param end_date End date in YYYY-MM-DD format
#' @param children Boolean flag to indicate whether data of the Orgunits c
#' children should be returned.
#' @description Gets a data value set for the provided paramaters
#' @return Only errors out if the file is not readable, does not exist or the
#' user cannot login to the server
#' 
GetDataValueSet <-
  function(org_unit,
           data_set,
           start_date,
           end_date,
           children = "true") {
    #TODO add error handling
    web_api_call <- paste0(
      getOption("baseurl"),
      "api/dataValueSets.csv?dataSet=",
      data_set,
      "&orgUnit=",
      org_unit,
      "&children=",
      children,
      "&startDate=",
      start_date,
      "&endDate=",
      end_date
    )
    #print(web_api_call)
    data <- web_api_call  %>%
      httr::GET() %>%
      httr::content(., "text")
    
    data <- data %>% stringr::str_replace(",deleted","") %>%
      readr::read_csv(col_names=TRUE, cols_only(dataelement = col_character(),
                                         period = col_character(),
                                         orgunit = col_character(),
                                         categoryoptioncombo = col_character(),
                                         attributeoptioncombo = col_character(),
                                         value = col_number()
      ))
    return(data)
  }

GetOrgUnitChildren <-
  function(org_unit) {
    #TODO add error handling
    end_point = "organisationUnits"
    filters = c(paste0("id:eq:", psnu))
    fields = "children"
    children <- getMetadata(end_point, filters, fields)[["children"]][[1]][["id"]]
    return(children)
  }

GetAnalytics <-
  function(technical_area, numerator_or_denominator, support_types,
           disaggregation_type, targets_or_results,organisation_units,
           dimensions_row=NULL,dimensions_col=NULL,period){

        if (!is.null(dimensions_row)) {
          dimensions_row <-  dimensions_row %>% paste0("&dimension=", ., collapse = "")
    }

    if (!is.null(dimensions_col)) {
      dimensions_col <- dimensions_col %>% paste0("&dimension=", ., collapse = "")
    }


    #TODO add error handling
    web_api_call <- paste0(
      getOption("baseurl"),
      "api/analytics.csv?",
      "dimension=ou:",  paste0(organisation_units,collapse = ";"),
      dimensions_row, dimensions_col,
      "&filter=LxhLO68FcXm:", technical_area,
      "&filter=lD2x0c8kywj:", numerator_or_denominator,
      "&filter=TWXpUVE2MqL:", paste0(support_types,collapse = ";"),
      "&filter=HWPJnUTMjEq:", disaggregation_type,
      "&filter=IeMmjHyBUpi:", targets_or_results,
      "&filter=pe:", period
    )
    #print(web_api_call)
    
    data <- web_api_call  %>%
      httr::GET() %>%
      httr::content(., "text")
    
    data <- data  %>%
      readr::read_csv(col_names=TRUE)
        return(data)
  }

add_name_col <- function(data_tib, column_str, end_point_str) {
  unique_items =   unique(data_tib[[column_str]])
  unique_items = paste0(unique_items, collapse = ",")
  item_name_id <-
    paste0(
      getOption("baseurl"),
      "api/",
      end_point_str,
      ".csv?paging=false&fields=name,id&filter=id:in:[",
      unique_items,
      "]"
    ) %>%
    httr::GET() %>%
    httr::content(., "text")  %>%
    readr::read_csv(col_names = TRUE)


  item_name_id <-
    item_name_id %>%
    dplyr::rename(!!paste0(column_str, "_name") := name) %>%
    dplyr::rename(!!column_str := id)

  data_tib <- dplyr::inner_join(mer_data, item_name_id)
  return(data_tib)
}
