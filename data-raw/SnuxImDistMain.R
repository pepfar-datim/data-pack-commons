devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)

library(datapackcommons)
library(datimutils)
library(dplyr)
datimutils::loginToDATIM("~/.secrets/datim.json")

cop_year = 2021

#' @title BuildDimensionList_DataPack(data_element_map_item, dim_item_sets, 
#' country_uid, mechanisms = NULL)
#' 
#' @description get list of dimensions (parameters) for analytics call to get data for SNUxIM 
#' distribution. Tightly coupled to DATIM as it contains some hard coded dimension uids 
#' for Funding Mechanism, technical area, num or denom, disagg type, support type, 
#' and type of organization unit. Also some hard coded dimension items for support type
#' @param data_element_map_item Single row of data_element_map being sliced and passed
#' @param dim_item_sets Dataframe containing all the dimension item sets e.g. datapackcommons::dim_item_sets
#' @param country_uid Country uid
#' @param mechanisms All historic mechanisms for the country filtered by id.
#' @param mil Set to True to pull the military data, false excludes military data when pulling PSNU
#' level data
#' When included the dimensions include psnu, mechanism, AND DSD/TA disaggregation.
#' When null psnu, mechanism and DSD/TA disaggregation are excluded giving country level totals.
#' @return  List of dimensions for the analytics call GetData_Analytics
BuildDimensionList_DataPack <- function(data_element_map_item, dim_item_sets, 
                                        country_uid, mechanisms = NULL,
                                        mil = FALSE){
  
  # prepare df of common dimensions and filters as expected by GetData_analytics  
  dimension_common <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "filter", data_element_map_item[[1,"dx"]],"dx", 
                    "filter", data_element_map_item[[1,"pe"]], "pe",
                    "dimension", country_uid, "ou",
                    "dimension", data_element_map_item[[1,"technical_area_uid"]], "LxhLO68FcXm",
                    "dimension", data_element_map_item[[1,"num_or_den_uid"]],"lD2x0c8kywj",
                    "dimension", data_element_map_item[[1,"disagg_type_uid"]],"HWPJnUTMjEq"
    )
  
  # prepare df of dimensions and filters as expected by GetData_analytics  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
    dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>% 
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>% 
    stats::na.omit() # there are some items in dim item sets with no source dimension
  
  if (is.null(mechanisms)){
    return(dplyr::bind_rows(dimension_common, dimension_disaggs))
  }
  
  
  dimension_mechanisms <- mechanisms["mechanism_uid"] %>% 
    dplyr::transmute(type = "dimension",
                     dim_item_uid = mechanism_uid,
                     dim_uid = "SH885jaRe0o")
  
  # remaining dimensions
  if (mil == FALSE) {  
    # need to select all org unit types EXCEPT military because it is 
    # possible for military to be below general PSNU level in org hierarchy
    invisible(capture.output(    non_mil_types_of_org_units <-
                                   datimutils::getDimensions("mINJi7rR1a6",
                                                             fields = "items[name,id]") %>%
                                   dplyr::filter(name != "Military") %>%
                                   .[["id"]]))

    
    tibble::tibble(type = "filter",
                   dim_item_uid = non_mil_types_of_org_units,
                   dim_uid = "mINJi7rR1a6") %>% 
    dplyr::bind_rows(tibble::tribble(~type, ~dim_item_uid, ~dim_uid, 
                  "dimension", "OU_GROUP-AVy8gJXym2D", "ou", # COP Prioritization SNU
                  "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                  "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL")) %>% 
    dplyr::bind_rows(dimension_mechanisms, dimension_disaggs, dimension_common)
    } else {
  tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                  "dimension", "OU_GROUP-nwQbMeALRjL", "ou", # military
                  "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                  "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
        dplyr::bind_rows(dimension_mechanisms, dimension_disaggs, dimension_common)
    }
}

GetFy21tMechs <- function(d2_session = dynGet("d2_default_session",
                                              inherits = TRUE)){

  
  #TODO modify format data for api function so I can make this call with getData_Analytics

    mech_codes <-
    datimutils::getCategories("SH885jaRe0o",
                                 fields = "categoryOptions[id,code]") %>%
    dplyr::rename(mechanism_uid = "id")
  
  mechs <- paste0(d2_session$base_url, "api/29/analytics.csv?dimension=SH885jaRe0o&dimension=ou:OU_GROUP-cNzfcPWEGSH;ybg3MO3hcf4&filter=pe:2020Oct&filter=dx:DE_GROUP-WTq0quAW1mf&displayProperty=SHORTNAME&outputIdScheme=UID") %>% 
    datapackcommons::RetryAPI("application/csv",
                              d2_session = d2_session) %>% 
    httr::content() %>% 
    readr::read_csv() %>%
    dplyr::select(-Value) %>% 
    setNames(c("mechanism_uid", "country_uid")) %>% 
    dplyr::left_join(mech_codes) %>% 
    dplyr::mutate(mechanism_code = datimutils::getCatOptions(mechanism_uid, fields = "code"))
  
  if(NROW(mechs) > 0){
    return(mechs)
  }
  # If I got here critical error
  stop("Unable to get 21T mechanisms")
}

GetFy22tMechs <- function(d2_session = dynGet("d2_default_session",
                                              inherits = TRUE)){
  
  #TODO modify format data for api function so I can make this call with getData_Analytics
  
  mechs <- paste0(d2_session$base_url, "api/29/analytics.csv?dimension=SH885jaRe0o&dimension=ou:OU_GROUP-cNzfcPWEGSH;ybg3MO3hcf4&filter=pe:2021Oct&filter=dx:DE_GROUP-QjkuCJf6lCs&displayProperty=SHORTNAME&outputIdScheme=UID") %>% 
    datapackcommons::RetryAPI("application/csv",
                              d2_session = d2_session) %>% 
    httr::content() %>% 
    readr::read_csv() %>%
    dplyr::select(-Value) %>% 
    setNames(c("mechanism_uid", "country_uid")) %>% 
    dplyr::mutate(mechanism_code = datimutils::getCatOptions(mechanism_uid, fields = "code"))
  
  if(NROW(mechs) > 0){
    return(mechs)
  }
  # If I got here critical error
  stop("Unable to get 22T mechanisms")
}

getSnuxIm_density <- function(data_element_map_item, 
                              dim_item_sets = datapackcommons::dim_item_sets, 
                              country_uid,
                              mechanisms){ 
  
  
  data <-  BuildDimensionList_DataPack(data_element_map_item, 
                                       dim_item_sets,
                                       country_uid,
                                       mechanisms["mechanism_uid"],
                                       mil = FALSE) %>% 
    datapackcommons::GetData_Analytics() %>% .[["results"]]

  data <-  BuildDimensionList_DataPack(data_element_map_item, 
                                       dim_item_sets,
                                       country_uid,
                                       mechanisms["mechanism_uid"],
                                       mil = TRUE) %>% 
    datapackcommons::GetData_Analytics() %>% .[["results"]] %>% 
    dplyr::bind_rows(data)
  
  if (NROW(data) == 0) return(NULL)
  
  # quick check that data disaggregated by psnu, mechanism, and support type sum to country total    
  checksum <- BuildDimensionList_DataPack(data_element_map_item,
                                          dim_item_sets,
                                          country_uid) %>%
    datapackcommons::GetData_Analytics() %>% .[["results"]] %>% .[["Value"]] %>% sum()
  
  if(sum(data$Value) > checksum){
    stop(paste("Internal Error: Disaggregated data greater than aggregated data in getSnuxIm_density function", 
               sum(data$Value), 
               checksum),"\n", data_element_map_item, " ")
  }
  if(sum(data$Value) < checksum){
    warning(paste("\n\nWARNING: Disaggregated data LESS than aggregated data in getSnuxIm_density function, Was PSNU level changed since last cop? ", 
                  "\n", data_element_map_item, " ",
                  "\n",datimutils::getOrgUnits(country_uid)),
            immediate. = TRUE)
  }
  
  disagg_sets  <-  c("age_set", 
                     "sex_set", 
                     "kp_set", 
                     "other_disagg") %>% 
    purrr::map(~dplyr::filter(dim_item_sets,
                              model_sets == data_element_map_item[[1, .]]))
  
  data <- purrr::reduce(disagg_sets,
                        datapackcommons::MapDimToOptions,
                        allocate = "distribute",
                        .init = data) %>% 
    dplyr::rename("mechanism_uid" = "Funding Mechanism") %>%
    dplyr::mutate(mechanism_code = datimutils::getCatOptions(mechanism_uid, 
                                                             fields = "code"),
                  mechanism_uid = datimutils::getCatOptionCombos(mechanism_code,
                                                                   by = code,
                                                                 fields = "id"),
                  fields = "uid") %>% 
    dplyr::mutate(indicator_code = data_element_map_item$indicator_code) %>%
    dplyr::rename("value" = "Value",
                  "psnu_uid" = "Organisation unit",
                  "type" = "Support Type") %>% 
    dplyr::select(suppressWarnings(dplyr::one_of("indicator_code", "psnu_uid",
                                "mechanism_uid", "mechanism_code", "type",
                                "age_option_name", "age_option_uid",
                                "sex_option_name", "sex_option_uid",
                                "kp_option_name", "kp_option_uid",
                                "value")))
  
  if("age_option_name" %in% names(data)){
    data$age_option_name[data$age_option_name == "<1"] <- "<01"
    data$age_option_name[data$age_option_name == "1-4"] <- "01-04"
    data$age_option_name[data$age_option_name == "5-9"] <- "05-09"
    data$age_option_name[data$age_option_name == "<= 2 months"] <- "<= 02 months"
    data$age_option_name[data$age_option_name == "2 - 12 months"] <- "02 - 12 months"
  }
  data$type[data$type == "cRAGKdWIDn4"] <- "TA"
  data$type[data$type == "iM13vdNLWKb"] <- "DSD"
  
  return(data)
}

process_country <- function(country_uid, mechs, snu_x_im_map){

  print(country_uid)
  # Get the mechanisms relevant for the specifc country being processed
  # cache options required for datimvalidation function to work.
  # cache age option reverts to original after calling datim validation
  mechs <-   dplyr::filter(mechs, country_uid == !!country_uid)
  if(NROW(mechs) == 0){return( tibble::tibble( indicator_code = character(),  
                                               psnu_uid = character(),     
                                               mechanism_code = character(),
                                               mechanism_uid = character(),
                                               type = character(),            
                                               age_option_name = character(), 
                                               age_option_uid = character(), 
                                               sex_option_name = character(), 
                                               sex_option_uid = character(),
                                               value = double(),
                                               kp_option_uid = character(), 
                                               kp_option_name = character(), 
                                               percent = double()))}
  
  # alply to call SiteDensity for each row of data_element_map (each target data element)
  # will have a historic distribution for each target, DSD/TA, and site given psnu/IM
  # alply uses parallel processing here 
  
  doMC::registerDoMC(cores = 3) #stopped using parallel due to warnings being hidden, 
  #must refactor to include
  data <-  plyr::adply(snu_x_im_map,
                     1, getSnuxIm_density,
                     datapackcommons::dim_item_sets,
                     country_uid,
                     mechs , .parallel = FALSE
                     , .expand = FALSE, .id = NULL) 
  if(NROW(data) == 0 || is.null(data)){return( tibble::tibble( indicator_code = character(),  
                                                     psnu_uid = character(),  
                                                     mechanism_code = character(),
                                                     mechanism_uid = character(),
                                                     type = character(),            
                                                     age_option_name = character(), 
                                                     age_option_uid = character(), 
                                                     sex_option_name = character(), 
                                                     sex_option_uid = character(),
                                                     value = double(),
                                                     kp_option_uid = character(), 
                                                     kp_option_name = character(), 
                                                     percent = double()))}
  
  data <-  data %>% 
    dplyr::group_by_at(dplyr::vars(-value)) %>% 
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup()

  if(!("kp_option_uid" %in% names(data))){
    data <- dplyr::mutate(data,
                  "kp_option_uid" = NA_character_,
                  "kp_option_name" = NA_character_)
    }
    
    dplyr::group_by(data,
                    indicator_code, 
                    psnu_uid, 
                    age_option_uid, 
                    sex_option_uid, 
                    kp_option_uid) %>%
    dplyr::mutate(percent = value/sum(value)) %>% 
    dplyr::ungroup()
}


mechs <-  switch(as.character(cop_year),
                 "2021" = GetFy21tMechs(),
                 "2022" = GetFy22tMechs())
fy_map <-  switch(as.character(cop_year),
                  "2021" = datapackcommons::Map21Tto22T,
                  "2022" = datapackcommons::Map22Tto23T)

country_details <-  datapackcommons::GetCountryLevels() %>% 
  # dplyr::filter(country_name == "Malawi") %>% 
  dplyr::arrange(country_name)

data <-  country_details[["id"]] %>% 
  purrr::map(process_country, mechs, fy_map) %>% 
  setNames(country_details$id)
  
#data$ODOymOOWyl0 <- process_country("ODOymOOWyl0", mechs) 


data_old = readr::read_rds(file.choose())
# data_old = data_old$lZsCb6y0KDX
data_old <- setNames(data_old, country_details$id)

non_nulls <- purrr::map_lgl(names(data), 
                            ~ !is.null(data[[.x]]) || !is.null(data_old[[.x]]))
names <-  names(data)[non_nulls]

purrr::map(names, ~try(dplyr::all_equal(data[[.x]],data_old[[.x]]))) %>% 
  setNames(datimutils::getOrgUnits(names))

deltas=purrr::map_df(names, ~try(dplyr::full_join(
  dplyr::bind_cols(data[[.x]], tibble::tribble(~new,1)),
  dplyr::bind_cols(data_old[[.x]], tibble::tribble(~old,1))))) %>% 
  dplyr::filter(is.na(old) | is.na(new)) 


if (cop_year == 2021){
  readr::write_rds(data,
                   paste0("/Users/sam/COP data/PSNUxIM_COP21_", lubridate::now("UTC"), ".rds"),
                   compress = c("gz"))
  readr::write_rds(data,
                   "/Users/sam/COP data/psnuxim_model_data_21.rds",
                   compress = c("gz"))
  file_name <- "psnuxim_model_data_21.rds"
} else if (cop_year == 2022){
  readr::write_rds(data,
                   "/Users/sam/COP data/PSNUxIM_COP22_", lubridate::now("UTC"), ".rds",
                   compress = c("gz"))
  readr::write_rds(data,
                   "/Users/sam/COP data/psnuxim_model_data_22.rds",
                   compress = c("gz"))
  file_name <- "psnuxim_model_data_22.rds"
}

Sys.setenv(
  AWS_PROFILE = "datapack-testing",
  AWS_S3_BUCKET = "testing.pepfar.data.datapack"
)

s3<-paws::s3()

r<-tryCatch({
  foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                     Body = paste0("/Users/sam/COP data/", s3_file_name),
                     Key = paste0("support_files/", s3_file_name))
  print("DATIM Export sent to S3", name = "datapack")
  TRUE
},
error = function(err) {
  print("DATIM Export could not be sent to  S3",name = "datapack")
  print(err, name = "datapack")
  FALSE
})

Sys.setenv(
  AWS_PROFILE = "datapack-prod",
  AWS_S3_BUCKET = "prod.pepfar.data.datapack"
)

s3<-paws::s3()

r<-tryCatch({
  foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                     Body = paste0("/Users/sam/COP data/", s3_file_name),
                     Key = paste0("support_files/", s3_file_name))
  print("DATIM Export sent to S3", name = "datapack")
  TRUE
},
error = function(err) {
  print("DATIM Export could not be sent to  S3",name = "datapack")
  print(err, name = "datapack")
  FALSE
})

s3$list_objects_v2(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                   Prefix = paste0("support_files/", s3_file_name)) %>% 
  purrr::pluck("Contents", 1, "LastModified")
