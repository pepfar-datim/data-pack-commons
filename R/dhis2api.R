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

