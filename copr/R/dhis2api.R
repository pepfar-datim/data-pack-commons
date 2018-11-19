LoadConfig <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    foo <- assertthat::assert_that(file.exists(config_path))
    s <- jsonlite::fromJSON(config_path)
    options("baseurl" = s$dhis$baseurl)
    options("config" = config_path)
    url <-
      URLencode(URL = paste0(getOption("baseurl"), "api/me"))
    #Logging in here will give us a cookie to reuse
    r <- httr::GET(url ,
             httr::authenticate(s$dhis$username, s$dhis$password),
             httr::timeout(60))
    assertthat::assert_that(r$status == 200L)
    print("Successfully logged in!")
    r <- httr::content(r, "text")
    me <- jsonlite::fromJSON(r)
    options("organisationUnit" = me$organisationUnits$id)
  } else {
    stop("You must specify a credentials file!")
  }
}

getMetadata <- function(end_point,
                        filters = NULL,
                        fields = NULL) {
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
  print(web_api_call)
  data <- web_api_call %>%  httr::GET() %>%
    httr::content(., "text")   %>%
    jsonlite::fromJSON()
  return(data[[end_point]])
}

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
    print(web_api_call)
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
           dimensions_row,dimensions_col,period){

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
      "dimension=ou:", organisation_units %>% paste0(collapse = ";"),
      dimensions_row, dimensions_col,
      "&filter=LxhLO68FcXm:", technical_area,
      "&filter=lD2x0c8kywj:", numerator_or_denominator,
      "&filter=TWXpUVE2MqL:", support_types %>% paste0(collapse = ";"),
      "&filter=HWPJnUTMjEq:", disaggregation_type,
      "&filter=IeMmjHyBUpi:", targets_or_results,
      "&filter=pe:", period
    )
    print(web_api_call)
    data <- web_api_call  %>%
      httr::GET() %>%
      httr::content(., "text")
    data <- data  %>%
      readr::read_csv(col_names=TRUE)



        return(data)
  }
