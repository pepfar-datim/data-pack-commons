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
getMetadata <- function(end_point, filters = NULL, fields = NULL) {
  
  url_filters=""
  url_fields=""
  
  if (!is.null(filters)) {
    url_filters <- paste0("&filter=", filters ) %>% URLencode()
  }
  
  if (!is.null(fields)) {
    url_fields <- paste0("&fields=", paste(fields,sep="",collapse=",")) %>% URLencode()
  }
  
  web_api_call <- paste0(getOption("baseurl"),
                         "api/",
                         end_point,
                         ".json?paging=false",
                         url_filters,
                         url_fields)
    r <- web_api_call %>%
    httr::GET()
    
    if (r$status_code == 200L) {
    httr::content(r, "text")   %>%
    jsonlite::fromJSON() %>%
    rlist::list.extract(.,end_point) } else {
      stop("Could not retreive endpoint")
    }
}

#' @export
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

  data_tib <- dplyr::inner_join(data_tib, item_name_id)
  return(data_tib)
}


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
