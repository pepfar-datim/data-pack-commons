# devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
#                   build = TRUE,
#                   upgrade = FALSE)

# NOTES ------------------------------------------------------------------------
# the following script generates the model for the SNIXUIM distribution

### Script Parameters ####################
# and then running through line ~ END SCRIPT
# WHEN RUNNING LOCALLY ALWAYS RENV.RESTORE AND THEN INITIATE CMD+SHIFT+B TO SEE CHANGES
# these are set whether to run locally or on posit server
cop_year <- 2024
compare <- TRUE
posit_server <- TRUE
#####

library(dplyr)
# posit publishing requires a reproducible library and since datapackcommons
# is not in the renv.lock file it is installed fresh from master since
# master always represents the valid branch to use
if (isTRUE(posit_server)) {
  devtools::install_github("https://github.com/pepfar-datim/data-pack-commons",
                           ref = "master",
                           upgrade = FALSE)

  # extract installed commit
  commit <-
    devtools::package_info("datapackcommons") %>%
    dplyr::filter(package == "datapackcommons") %>%
    dplyr::pull(source) %>%
    stringr::str_extract(., "(?<=@)\\w{7}")

  print(paste0("Installed Latest datapackcommons, using commit: ", commit))
} else {

  # extract commit of the local version you are using
  commit <-
    devtools::package_info("datapackcommons") %>%
    dplyr::filter(package == "datapackcommons") %>%
    dplyr::pull(source) %>%
    stringr::str_extract(., "(?<=@)\\w{7}")
  require(datapackcommons)
}

library(datimutils)

# login to datim
# if using posit server we pull env vars
if (isTRUE(posit_server)) {
  datimutils::loginToDATIM(
    username = Sys.getenv("UN"),
    password = Sys.getenv("PW"),
    base_url = Sys.getenv("BASE_URL")
  )
} else {
  datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                  "datim.json"))
}

# FUNCTIONS -------------------------------------------------------------------

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
#' @return  List of dimensions for the analytics call datimutils::getAnalytics
BuildDimensionList_DataPack <- function(data_element_map_item, dim_item_sets,
                                        country_uid, mechanisms = NULL,
                                        mil = FALSE) {

  # prepare base list input for dimensions and filters fed to getAnalytics
  dimensions_common_list <- list(
    ou %.d% country_uid,
    dx %.f% data_element_map_item[[1, "dx"]],
    pe %.f% data_element_map_item[[1, "pe"]],
    "LxhLO68FcXm" %.d% data_element_map_item[[1, "technical_area_uid"]],
    "lD2x0c8kywj" %.d% data_element_map_item[[1, "num_or_den_uid"]],
    "HWPJnUTMjEq" %.d% data_element_map_item[[1, "disagg_type_uid"]],
    retry = 3,
    timeout = 300
  )

  # pull dimension disaggs as data frame and translate to list
  dimensions_disaggs_list <- dim_item_sets %>%
    dplyr::mutate(type = "dimension") %>%
    dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>%
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>%
    stats::na.omit() %>%
    datapackcommons::translateDims(.)

  # if mechanisms are null return appended list of dimensions_common and dimensions_disaggs
  if (is.null(mechanisms)) {
    return(append(
      dimensions_common_list,
      dimensions_disaggs_list
    ))
  }

  # build dimensions and translate to list
  dimensions_mechanisms_list <- mechanisms["mechanism_uid"] %>%
    dplyr::transmute(type = "dimension",
                     dim_item_uid = mechanism_uid,
                     dim_uid = "SH885jaRe0o") %>%
    datapackcommons::translateDims(.)

  # remaining dimensions
  if (mil == FALSE) {
    # when mil == FALSE need to select all org unit types EXCEPT military because it is
    # possible for military to be below general PSNU level in org hierarchy
    invisible(
      capture.output(
        non_mil_types_of_org_units <-
          datimutils::getDimensions("mINJi7rR1a6", # Type of organisational unit org group set
                                    fields = "items[name,id]") %>%
          dplyr::filter(name != "Military") %>%
          .[["id"]]))


    # create result item
    res <-
      c(
        dimensions_common_list,
        dimensions_mechanisms_list,
        dimensions_disaggs_list,
        list(
          "mINJi7rR1a6" %.f% non_mil_types_of_org_units,
          "TWXpUVE2MqL" %.d% c("iM13vdNLWKb", "cRAGKdWIDn4") #dsd and ta support types
        )
      )

    # add ou dimension
    res[grepl("dimension=ou:", res)] <- paste0(res[grepl("dimension=ou:", res)],
                                               ";", "OU_GROUP-AVy8gJXym2D") # COP Prioritization SNU

    res

  } else {

    res <-
      c(
        dimensions_common_list,
        dimensions_mechanisms_list,
        dimensions_disaggs_list,
        list(
          "TWXpUVE2MqL" %.d% c("iM13vdNLWKb", "cRAGKdWIDn4") #dsd and ta support types
        )
      )

    # add ou dimension
    res[grepl("dimension=ou:", res)] <- paste0(res[grepl("dimension=ou:", res)], ";", "OU_GROUP-nwQbMeALRjL") # military

    res
  }

}

# get a list of mechs that actually have associated data for the FY targets
getMechsList <- function(cop_year,
                         d2_session = dynGet("d2_default_session",
                                             inherits = TRUE)) {
  assertthat::assert_that(cop_year %in% c(2021,
                                          2022,
                                          2023,
                                          2024))
  #if (cop_year == 2023) #{cop_year = 2022} # temporary code to get mechs for last year for initial cop23 dev
    # Wed Feb 22 13:29:39 2023 We should probably remove line 136 now.
  de_group <- dplyr::case_when(
    cop_year == 2021 ~ "WTq0quAW1mf", #"2021 MER Targets"
    cop_year == 2022 ~ "QjkuCJf6lCs",  #"2022 MER Targets",
    cop_year == 2023 ~ "OuKFZzVk6gr",  #"2023 MER Targets",
    cop_year == 2024 ~ "TXAVaM4oYMd",  #"2024 MER Targets",
  )

  mechs <- datimutils::getAnalytics(
    dx_f = paste0("DE_GROUP-", de_group),
    ou = paste0("OU_GROUP-",
                datimutils::getOrgUnitGroups("Country", by = name)),
    pe_f = paste0(cop_year - 1, "Oct"),
    "SH885jaRe0o" %.d% "all",
    timeout = 180) %>%
    dplyr::select(mechanism_uid = "Funding Mechanism",
                  country_uid = "Organisation unit") %>%
    dplyr::mutate(mechanism_code =
                    datimutils::getCatOptions(mechanism_uid, fields = "code"))

  if (NROW(mechs) > 0) {
    return(mechs)
  }
  # If I got here critical error
  stop("Unable to get any mechanisms")
}


getSnuxIm_density <- function(data_element_map_item,
                              dim_item_sets = datapackcommons::dim_item_sets,
                              country_uid,
                              mechanisms) {


      # first get non-mil data
      data1 <-  BuildDimensionList_DataPack(data_element_map_item,
                                            dim_item_sets,
                                            country_uid,
                                            mechanisms["mechanism_uid"],
                                            mil = FALSE) %>%
        do.call(getAnalytics, .)

      # now get mil data and combine with non-mil
      data2 <-  BuildDimensionList_DataPack(data_element_map_item,
                                            dim_item_sets,
                                            country_uid,
                                            mechanisms["mechanism_uid"],
                                            mil = TRUE) %>%
        do.call(getAnalytics, .)

  data <- dplyr::bind_rows(data1, data2)
  rm(data1, data2)

  # early return if no data
  if (NROW(data) == 0) return(NULL)

  # quick check that data disaggregated by psnu, mechanism, and support type sum to country total

  # get country level totals (checksum)
  checksum <- BuildDimensionList_DataPack(data_element_map_item,
                                          dim_item_sets,
                                          country_uid) %>%
    do.call(getAnalytics, .) %>%
    .[["Value"]] %>%
    sum()

  if (sum(data$Value) > checksum) { # this is hard stop, we must be double counting data somehow
    stop(paste("Internal Error: Disaggregated data greater than aggregated data in getSnuxIm_density function",
               sum(data$Value),
               checksum), "\n", data_element_map_item, " ")
  }
  if (sum(data$Value) < checksum) { # if PSNU level in org hierarchy was lowered may be CORRECTLY
                                    # missing some historic target data
    warning(paste("\n\nWARNING: Disaggregated data LESS than aggregated data in getSnuxIm_density function,",
                  " Was PSNU level changed since last cop? ",
                  "\n", data_element_map_item, " ",
                  "\n", datimutils::getOrgUnits(country_uid)),
            immediate. = TRUE)
  }

# create list object with element containing dim_item_sets rows
# for each disagg type -- age, sex, kp, other
  disagg_sets  <-  c("age_set",
                     "sex_set",
                     "kp_set",
                     "other_disagg") %>%
    purrr::map(~dplyr::filter(dim_item_sets,
                              model_sets == data_element_map_item[[1, .]]))

# aggregate data based on dim_item_sets

  data <- purrr::reduce(disagg_sets,
                        datapackcommons::MapDimToOptions,
                        allocate = "distribute", # we always distribute data for PSNUxIM
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

  if ("age_option_name" %in% names(data)) {
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

 process_country <- function(country_uid, mechs, snu_x_im_map) {

  print(country_uid)
  # Get the mechanisms relevant for the specific country being processed
  mechs <-   dplyr::filter(mechs, country_uid == !!country_uid)
  if (NROW(mechs) == 0) {
    return(tibble::tibble(indicator_code = character(),
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
                            percent = double()))
  }

  # alply to call SiteDensity for each row of data_element_map (each target data element)
  # will have a historic distribution for each target, DSD/TA, and site given psnu/IM
  # alply uses parallel processing here

  doMC::registerDoMC(cores = 3) #stopped using parallel due to warnings being hidden,
  # must refactor to output warnings in some way
  # important because getSnuxIm_density outputs a warning when checksum doesn't reconcile
  data <-  plyr::adply(snu_x_im_map,
                     1, getSnuxIm_density,
                     datapackcommons::dim_item_sets,
                     country_uid,
                     mechs, .parallel = FALSE,
                     .expand = FALSE, .id = NULL)
  # when no relevant data available
  if (NROW(data) == 0 || is.null(data)) {
    return( # empty tibble with expected columns
      tibble::tibble(indicator_code = character(),
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
                     percent = double()))
    }

  data <- data %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if (!("kp_option_uid" %in% names(data))) {
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
    dplyr::mutate(percent = value / sum(value)) %>%
    dplyr::ungroup()
}

#### End Functions

### Start script execution

 # SCRIPTING -------------------------------------------------------------------

mechs <-  getMechsList(cop_year)

fy_map <-  switch(as.character(cop_year),
                  "2021" = datapackcommons::Map21Tto22T,
                  "2022" = datapackcommons::Map22Tto23T,
                  "2023" = datapackcommons::Map23Tto24T,
                  "2024" = datapackcommons::Map24Tto25T
                  )

# pull list of countries to iterate through
country_details <-  datimutils::getOrgUnitGroups("Country", name, fields = "organisationUnits[name,id]") %>%
  dplyr::arrange(name) %>%
  select(country_name = name, id) #%>%
  #dplyr::filter(country_name == "South Africa")

# start process of collecting api data for every country
data_new <-  country_details[["id"]] %>%
  purrr::map(process_country, mechs, fy_map, .progress = list(
    type = "iterator",
    format = "Calculating {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE)) %>%
  setNames(country_details$id)

#data$ODOymOOWyl0 <- process_country("ODOymOOWyl0", mechs)

#### COMPARISON - AUTOMATED ----
# when compare is set to TRUE we compare this run against the
# latest production psnuxim model in test S3
# FOR DEVELOPMENT PURPOSES RUN CODE MANUALLY

if (compare == FALSE) {

  print("done")

} else {

  # file name in S3
  cop_year_end <- substr(cop_year, 3, 4)
  file_name <- paste0("psnuxim_model_data_", cop_year_end, ".rds")

  # explicitly set to make sure we are in test
  Sys.setenv(
    AWS_PROFILE = "datapack-testing",
    AWS_S3_BUCKET = "testing.pepfar.data.datapack"
  )

  s3 <- paws::s3()

  # retrieve the production psnuxim model
  # and decode, error out if there is an issue
  r <- tryCatch({
    s3_download <- s3$get_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                                 Key = paste0(
                                   "support_files/",
                                   file_name
                                   )
                                 )

    # write output to file
    writeBin(s3_download$Body, con = file_name)

    # extract data
    data_old <- s3_download$Body %>% rawConnection() %>% gzcon %>% readRDS
    print("psnuxim model read from S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    print("psnuxim model could not be read from S3", name = "datapack")
    print(err, name = "datapack")
    FALSE
  })

  #data_old <- readr::read_rds(file.choose())

  deltas <- datapackcommons::diffSnuximModels(
    data_old,
    data_new,
    full_diff = TRUE
  )

  print(paste0("The difference between the production PSNXUIM model in TEST S3 and the new model is: ", nrow(deltas)))

  #### DOWNLOADABLE NEW MODEL ----
  # if the new model has a diff we will create a filename
  # and prepare the file for download availability in rmarkdown
  # file can then be downloaded and reviewed along with report
  # and moved manually to s3 test/prod
  if (NROW(deltas) > 0) {

    # save flattened version with data to disk
    cop_year_end <- substr(cop_year, 3, 4)
    new_psx_file_name <- paste0("psnuxim_model_data_", cop_year_end, "_", lubridate::today(), "_", commit)

    # if run locally with local params set file is automatically saved
    if (isFALSE(posit_server)) {
      readr::write_rds(data,
                       paste0(new_psx_file_name, ".rds")
                       , compress = c("gz"))

    }
  }

  #### CLEANUP ----
  # using paws s3 we get and dump latest datapack model into our file system
  # here we make sure to get rid of it locally
  # Check if the file exists
  if (file.exists(file_name)) {
    # Delete the file
    file.remove(file_name)
    print(paste0(file_name, " LOCALLY deleted successfully."))
  } else {
    print(paste0(file_name, "File does not exist."))
  }



}

#### END SCRIPT --------------------------------------------------------------------

#### LEGACY AND TEST/PROD TRANSFER ----
# got rid of 22 and below
# this is legacy code as well as code for
# production transfer purposes
# if you download the model from the report, to update simply manually update
# the name of the model as needed and place it in OUTPUT_LOCATION VARIABLE

# if (cop_year == 2023){
#   readr::write_rds(data,
#                    paste0("/Users/sam/COP data/PSNUxIM_COP23_", lubridate::today(), ".rds"),
#                    compress = c("gz"))
#   readr::write_rds(data,
#                    "/Users/sam/COP data/psnuxim_model_data_23.rds",
#                    compress = c("gz"))
#   file_name <- "psnuxim_model_data_23.rds"
# } else if (cop_year == 2024){
#   readr::write_rds(data,
#                    paste0("/Users/sam/COP data/PSNUxIM_COP24_", lubridate::today(), ".rds"),
#                    compress = c("gz"))
#   readr::write_rds(data,
#                    "/Users/sam/COP data/psnuxim_model_data_24.rds",
#                    compress = c("gz"))
#   file_name <- "psnuxim_model_data_24.rds"
# }

# edit this output location to where ever your file is
# output_location <- "~/COP data/COP24 Update/"
# file_name <- new_psx_file_name
#
# Sys.setenv(
#   AWS_PROFILE = "datapack-testing",
#   AWS_S3_BUCKET = "testing.pepfar.data.datapack"
# )
#
# s3<-paws::s3()
#
# r<-tryCatch({
#   foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
#                      Body = paste0(output_location, file_name),
#                      Key = paste0("support_files/", file_name))
#   print("DATIM Export sent to S3", name = "datapack")
#   TRUE
# },
# error = function(err) {
#   print("DATIM Export could not be sent to  S3",name = "datapack")
#   print(err, name = "datapack")
#   FALSE
# })
#
# Sys.setenv(
#   AWS_PROFILE = "datapack-prod",
#   AWS_S3_BUCKET = "prod.pepfar.data.datapack"
# )
#
# s3<-paws::s3()
#
# r<-tryCatch({
#   foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
#                      Body = paste0(output_location, file_name),
#                      Key = paste0("support_files/", file_name))
#   print("DATIM Export sent to S3", name = "datapack")
#   TRUE
# },
# error = function(err) {
#   print("DATIM Export could not be sent to  S3",name = "datapack")
#   print(err, name = "datapack")
#   FALSE
# })
#
# s3$list_objects_v2(Bucket = Sys.getenv("AWS_S3_BUCKET"),
#                    Prefix = paste0("support_files/", file_name)) %>%
#   purrr::pluck("Contents", 1, "LastModified")
