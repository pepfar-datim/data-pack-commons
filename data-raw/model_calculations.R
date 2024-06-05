### Script Parameters ####################
# this script can be run manually by setting the params below
# and then running through line ~ END SCRIPT
# WHEN RUNNING LOCALLY ALWAYS RENV.RESTORE AND THEN INITIATE CMD+SHIFT+B TO SEE CHANGES
# these are set whether to run locally or on posit server
cop_year <- 2024
compare <- TRUE # default to true for full run
posit_server <- TRUE # default to true as it runs on server
#####

library(magrittr)
# posit publishing requires a reproducible library and since datapackcommons
# is not in the renv.lock file it is installed fresh from master since
# master always represents the valid branch to use
if (isTRUE(posit_server)) {
  devtools::install_github("https://github.com/pepfar-datim/data-pack-commons",
                           ref = "master",
                           upgrade = FALSE)

  # extract installed commit when running on server
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
require(datapackr)
require(datimutils)
require(magrittr)
require(tidyverse)
require(jsonlite)
require(lubridate)
require(rlang)
require(assertthat)
require(foreach)

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


# Countries to include in model, usually all
# Turkmenistan has no planning/priortization level
operating_units <- datimutils::getOrgUnitGroups("Country", name, fields = "organisationUnits[name,id]") %>%
  filter(name != "Turkmenistan") %>%
  dplyr::arrange(name) %>%
  select(country_name = name, id)

#' @title RenameAnalyticsColumns
#' @description Convert columns names from analytics output to names
#' used in this package
#' @param data dataframe with DATIM analytics output
#' @return data with renamed columns
RenameAnalyticsColumns <- function(data) {
  data %>%
    dplyr::rename(!!c("value" = "Value", indicator_uid = "Data",
                      "org_unit_uid" = "Organisation unit",
                      "period" = "Period"))
}


#' @title GetData
#' @description Wrapper for calling GetData_Datapack with plyr::alply
#' @param indicator_parameters details for a single analytics call originally coming from
#' data_required
#' @param ou_uid organization unit (country) uid for the analytics call
#' @param dim_item_sets disaggregation for the data pack generaly datapackcommons::dim_item_sets
#' @return return the indicator parameters dataframe (row) with the analytics output
#' to be reassembled into a full dataframe by alply
GetData <- function(indicator_parameters,
                    ou_uid,
                    dim_item_sets) {

  indicator_parameters <-  dplyr::as_tibble(indicator_parameters)

  analytics_output <- datapackcommons::GetData_DataPack(indicator_parameters,
                                                        ou_uid,
                                                        dim_item_sets = dim_item_sets)

  return(dplyr::mutate(indicator_parameters,
                       analytics_output = list(analytics_output)))
}


AggByAgeSexKpOu2 <- function(data) {
  #to do add assertions must include value and org unit columns
  # warning suppressed because one_of throws warning if named column is missing
  aggregated_data <-
    suppressWarnings(
      dplyr::select(data, dplyr::one_of(c("sex_option_uid", "age_option_uid", "kp_option_uid",
                                        "org_unit_uid", "value")))) %>%
    dplyr::group_by_at(dplyr::vars(-value)) %>%
    summarise(count = dplyr::n(), minimum = min(value),
              maximum = max(value), value = sum(value)) %>%
    ungroup()

  if (max(aggregated_data$count) == 1) { # nothing aggregated
    return(list(processed = data, was_aggregated = FALSE))
  } else {
    return(list(processed = aggregated_data, was_aggregated = TRUE,
         aggregations = filter(aggregated_data, count > 1)))
  }
}

ProcessDataRequiredRow <- function(data_spec, dim_item_sets) {
  SelectAndStripPrefix <- function(prefix, df) {
    dplyr::select(df, dplyr::starts_with(prefix, ignore.case = FALSE)) %>%
      dplyr::rename_all(.funs = stringr::str_remove, pattern = prefix)
  }

  assertthat::assert_that(NROW(data_spec) == 1)

  # Return list assumed to start out with a named list of the components to process,
  # no other list elements permissible
  if (is.na(data_spec$calculation)) {
    return_list <- list(A = NULL)
  } else {
    return_list <- list(A = NULL, B = NULL)
  }

  for (component in names(return_list)) {
    prefix <- paste0(component, ".")

    component_data <- SelectAndStripPrefix(prefix, data_spec)
    analytics_results <- component_data[[1, "analytics_output"]][[1]]$results
    analytics_api_call <- component_data[[1, "analytics_output"]][[1]]$api_call
    #analytics_time <- lubridate::as_datetime(component_data[[1,"analytics_output"]]$time)


    if (is.null(analytics_results)) {
      # adding empty version of minimum output columns to cop_data
      return_list[[component]][["processed"]] <- tibble::tibble(indicator_uid = character(),
                                                                org_unit_uid = character(),
                                                                period = character(),
                                                                age_option_uid = character(),
                                                                sex_option_uid = character(),
                                                                kp_option_uid = character(),
                                                                value = double())
      next
    }

      if (NROW(analytics_results) == 0) {
      # adding empty version of minimum output columns to cop_data
      return_list[[component]][["processed"]] <-  analytics_results %>%
        RenameAnalyticsColumns() %>%
        mutate(age_option_uid = org_unit_uid, sex_option_uid = org_unit_uid, kp_option_uid = org_unit_uid)
      # cop_data[[operating_unit$id]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]] <- analytics_output
      # # TODO add log or warning file
      next
    }

    age_set <- dim_item_sets %>% filter(model_sets == component_data[["age_set"]])
    sex_set <- dim_item_sets %>% filter(model_sets == component_data[["sex_set"]])
    kp_set <- dim_item_sets %>% filter(model_sets == component_data[["kp_set"]])

    mapped_data <- list(analytics_results,
                        age_set,
                        sex_set,
                        kp_set) %>%
      purrr::reduce(datapackcommons::MapDimToOptions, allocate = data_spec$allocate) %>%
      RenameAnalyticsColumns()

    component_data <- AggByAgeSexKpOu2(mapped_data)
    if (component_data$was_aggregated == TRUE) {
      assertthat::assert_that(data_spec$allocate == "distribute")
    }

    return_list[[component]] <- c(component_data,
                                  "api_call" = analytics_api_call
                                  #,
     #                             "time" = as.POSIXct(analytics_time,
                                   #                   origin = "1970-01-01",
    #                                                  tz = "UTC")
    )

  }

  if (is.na(data_spec[[1, "calculation"]]) && data_spec[[1, "type"]] == "Numeric") {
    return_list[["results"]] <- return_list[["A"]][["processed"]] %>% mutate(value = round(value))
    return(return_list)
  } else if (is.na(data_spec[[1, "calculation"]]) && data_spec[[1, "type"]] == "Percent") {
    return_list[["results"]] <- return_list[["A"]][["processed"]] %>% mutate(value = value / 100)
    return(return_list)
  } else { # performing extra calculation to combine components A and B

# warning suppressed because one_of throws warning if named column is missing
    A <-
      suppressWarnings(dplyr::select(return_list[["A"]][["processed"]],
                                     dplyr::one_of(c("sex_option_uid", "age_option_uid", "kp_option_uid",
                                                     "org_unit_uid", "value")))
                       ) %>%
      dplyr::rename("A" = "value")


    B <-  suppressWarnings(dplyr::select(return_list[["B"]][["processed"]],
                                         dplyr::one_of(c("sex_option_uid", "age_option_uid", "kp_option_uid",
                                                         "org_unit_uid", "value")))
                           ) %>%
      dplyr::rename("B" = "value")

    joined_data  <-  dplyr::full_join(A, B)

# After join replace NAs with the appropriate value (0 or NA) based on indicator config
    joined_data$A[is.na(joined_data$A)] <- data_spec$A.value_na
    joined_data$B[is.na(joined_data$B)] <- data_spec$B.value_na

    calculated_data <-  data_spec$calculation %>%
      str_replace_all("A", "joined_data$A") %>%
      str_replace_all("B", "joined_data$B") %>%
      rlang::parse_expr() %>%
      eval() %>%
      mutate(joined_data, value = .)


    calculated_data$value[is.infinite(calculated_data$value) | is.nan(calculated_data$value)] <- NA
    if (data_spec[[1, "type"]] == "Numeric") {
      return_list[["results"]] <- dplyr::mutate(calculated_data,
                                                value = round(value))
    } else {
      return_list[["results"]] <- calculated_data
    }

    return(return_list)
  }
}

diffDataPackModels <- function(model_old,
                               model_new,
                               full_diff = TRUE) {
# only include countries present in both files
  if (full_diff) {
    countries <- dplyr::union(names(model_old),
                              names(model_new))
  } else {
    countries <- dplyr::intersect(names(model_old),
                                  names(model_new))
  }

  model_old_filtered <- dplyr::bind_rows(model_old[countries]) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = round(value, 5)) %>%
    rename(value.old = value) %>%
    dplyr::select(-period)
  model_new_filtered <- dplyr::bind_rows(model_new[countries]) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = round(value, 5)) %>%
    rename(value.new = value) %>%
    dplyr::select(-period)

diff <- full_join(model_old_filtered, model_new_filtered) %>%
  dplyr::mutate(diff = value.new - value.old)

ancestors <- datimutils::getOrgUnits(diff$psnu_uid, fields = "ancestors[name]")
diff <- dplyr::mutate(diff,
                        psnu = datimutils::getOrgUnits(psnu_uid),
                        ou = purrr::map_chr(ancestors, purrr::pluck, 1, 3, .default = NA_character_),
                        snu1 = purrr::map_chr(ancestors, purrr::pluck, 1, 4, .default = NA_character_))

  deltas  <-  diff %>%
    filter(value.new != value.old |
             is.na(value.new) | is.na(value.old))

  matched  <-  diff %>%
    filter(value.new == value.old)

# convert some uids to names for readability of diff
# Thrice repeated code could be a function, but passing column name to the function
# seems to make code harder rather than easier to maintain

  if (!(all(is.na(deltas$age_option_uid)))) {
    deltas_split <- split(deltas, is.na(deltas$age_option_uid))
    deltas_split$`FALSE` <- dplyr::mutate(deltas_split$`FALSE`,
                                          age_option = datimutils::getCatOptions(age_option_uid))
    deltas <- dplyr::bind_rows(deltas_split)
  }

  if (!(all(is.na(deltas$sex_option_uid)))) {
    deltas_split <- split(deltas, is.na(deltas$sex_option_uid))
    deltas_split$`FALSE` <- dplyr::mutate(deltas_split$`FALSE`,
                                          sex_option = datimutils::getCatOptions(sex_option_uid))
    deltas <- dplyr::bind_rows(deltas_split)
  }

  if (!(all(is.na(deltas$kp_option_uid)))) {
    deltas_split <- split(deltas, is.na(deltas$kp_option_uid))
    deltas_split$`FALSE` <- dplyr::mutate(deltas_split$`FALSE`,
                                          kp_option = datimutils::getCatOptions(kp_option_uid))
    deltas <- dplyr::bind_rows(deltas_split)
  }

  return(list(deltas = deltas, matched = matched))
}

# initialize cop_data list for the model
 cop_data <- list()
# get impatt.priority_snu for each PSNU
# we use get data value sets to pull raw data instead of the analytics endpoint
# because the priortization level cannot/should not be aggregated

 priority_snu_data <-
   datimutils::getDataValueSets(c("dataElementGroup", "period", "orgUnitGroup"),
                               c("ofNbyQgD9xG", paste0(cop_year - 1, "Oct"), "AVy8gJXym2D")) %>%
   dplyr::select(org_unit_uid = orgUnit, value) %>%
   dplyr::mutate(value = as.double(value))

# get data to populat DREAMS_SNU.Flag
# PSNUs that are an dreams SNU or contain on or more DREAMS SNUs are flagged
# dreams_psnus are those that should be flagged
 dreams_snu_path_members <-
   datimutils::getOrgUnitGroups("mRRlkbZolDR", # Dreams SNUs
                                fields = "organisationUnits[path]") %>%
   dplyr::mutate(path = stringr::str_split(path, "/")) %>%
   unlist() %>%
   unique() %>%
   stringi::stri_remove_empty()

 dreams_psnus <- datimutils::getOrgUnitGroups("AVy8gJXym2D", # PSNUs
                                             fields = "organisationUnits[id,path]") %>%
   dplyr::filter(id %in% dreams_snu_path_members) %>%
   dplyr::mutate(value = 1, org_unit_uid = id)

# get local copy of package config file with disaggregations
dim_item_sets <- datapackcommons::dim_item_sets

# for each ou
for (ou_index in seq_len(NROW(operating_units))) {
  # start with fresh local copy of data_required.csv
  data_required <-  datapackcommons::data_required
  operating_unit <-  dplyr::slice(operating_units, ou_index)
  cop_data[[operating_unit$id]] <- list("ou_name" = operating_unit$country_name,
                                       "ou_psnu_level" = operating_unit$prioritization_level)

  print(operating_unit$country_name)
  print(lubridate::now())

# separate the A and B indicators defined in data_required for individual analytics calls
# Only include unique sets of parameters

  indicator_parameters <- datapackcommons::StackPrefixedCols(data_required, c("A.", "B.")) %>%
    unique() %>%
    filter(!is.na(dx_id)) # most B indicators are empty, filter them out

  doMC::registerDoMC(cores = 4) # or however many cores you have access to

# Make the analytics calls for the data required
# each row of indicator parameters contains the parameters for an analytics call
  analytics_output <- plyr::adply(indicator_parameters,
                                  1,
                                  GetData,
                                  operating_unit$id,
                                  dim_item_sets,
                                  .parallel = TRUE)

# Rename the columns of analytics output with prefix {A, B} to have matching column names in joins
  data_required <- analytics_output %>%
  dplyr::rename_all(.funs = function(x) paste0("A.", x)) %>%
    dplyr::left_join(data_required, .)
  data_required <- analytics_output %>%
    dplyr::rename_all(.funs = function(x) paste0("B.", x)) %>%
    dplyr::left_join(data_required, .)

#   for each line in data required
  for (data_required_index in seq_len(NROW(data_required))) {

    data_spec <-  dplyr::slice(data_required, data_required_index)
    cop_data[[operating_unit$id]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]] <-
      ProcessDataRequiredRow(data_spec, dim_item_sets)
  }

# dropping any impatt.priority_snu data that isn't raw data
  cop_data[[operating_unit$id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.T_1"]][["results"]] <-
    cop_data[[operating_unit$id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.T_1"]][["results"]] %>%
    dplyr::inner_join(priority_snu_data)

  cop_data[[operating_unit$id]][["AGYW"]][["DREAMS_SNU.Flag"]][["results"]] <-
    dplyr::filter(dreams_psnus, stringr::str_detect(path, operating_unit$id)) %>%
    dplyr::select(org_unit_uid, value)


# TODO bind rows to create a flat file
  }

print(lubridate::now())

#### COMPARISON - AUTOMATED ----
# when compare is set to TRUE we compare this run against the
# latest production datapack model in test S3
# FOR DEVELOPMENT PURPOSES RUN CODE MANUALLY

if (compare == FALSE) {

  print("done")

} else {

  # file name in S3
  file_name <- "datapack_model_data.rds"

  # explicitly set to make sure we are in test
  Sys.setenv(
    AWS_PROFILE = "datapack-testing",
    AWS_S3_BUCKET = "testing.pepfar.data.datapack"
  )

  s3 <- paws::s3()

  # retrieve the production datapack model
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
    model_old <- s3_download$Body %>% rawConnection() %>% gzcon %>% readRDS
    print("datapack model read from S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    print("datpack model could not be read from S3", name = "datapack")
    print(err, name = "datapack")
    FALSE
  })


  # flatten the new datapack model run data and produce diff
  model_new <- datapackcommons::flattenDataPackModel_21(cop_data)
  diff <- diffDataPackModels(model_old = model_old
                             #file.choose() %>% readr::read_rds(),
                             , model_new = model_new
                             # , model_new = file.choose() %>% readr::read_rds()
                             , full_diff = TRUE)

  deltas <- diff$deltas
  print(paste0("The difference between the production datapack model in TEST S3 and the new model is: ", nrow(deltas)))


  # creates a summary of matches for later use
  delta_summary <-  dplyr::group_by(deltas, indicator_code, ou) %>%
    dplyr::summarise(count_delta = dplyr::n())
  indicators_w_delta <- deltas$indicator_code %>% unique()

  matched_summary <- dplyr::group_by(diff$matched, indicator_code, ou) %>%
    dplyr::summarise(count_matched = dplyr::n(),
                     sum_matches = sum(value.old,
                                       na.rm = TRUE))
  summary <- dplyr::full_join(delta_summary, matched_summary) %>%
    dplyr::filter(indicator_code %in% indicators_w_delta) %>%
    dplyr::arrange(indicator_code)

  # create a more comprehensive summary for later use
  deltas_summary <- dplyr::group_by(deltas, indicator_code, ou) %>%
    dplyr::summarise(
      count_total = dplyr::n(),
      count_old_nas = sum(is.na(value.old)),
      count_new_nas = sum(is.na(value.new)),
      count_missing_psnu = sum(is.na(psnu)),
      count_missing_ou = sum(is.na(ou))
    ) %>%
    mutate(indicator_type = case_when(grepl("\\T_1$", indicator_code) ~ "TARGET",
                                      grepl("\\.R$", indicator_code)  ~ "RESULTS",
                                      grepl("\\.Yield$", indicator_code)  ~ "RESULTS_Y",
                                      grepl("\\.Share$", indicator_code)  ~ "TARGETS_S",

    )) %>%
    #mutate(indicator_type = ifelse(is.na(indicator_type), "YIELD", indicator_type)) %>%
    arrange(ou, indicator_code)

  # create a dataframe from the new model
  new_model_binded <- bind_rows(model_new)

  # label model data as present based on value
  new_model_binded_e <-
    new_model_binded %>%
    mutate(
      has_data = ifelse(!is.na(value), TRUE, FALSE)
    ) %>%
    mutate(has_data = replace(has_data, value == 0, FALSE))

  # pivot schema disaggs
  valid_schema_combos <- datapackcommons::pivotSchemaCombos(cop_year = 2024)

  # create a summary of combos present in datapack schema but not in data
  missing_schema_combos <- anti_join(
    valid_schema_combos,
    new_model_binded_e %>%
      filter(has_data == TRUE) %>%
      select(-value, -psnu_uid, -period) %>%
      distinct()
  )

  #### DOWNLOADABLE NEW MODEL ----
  # if the new model has a diff we will create a filename
  # and prepare the file for download availability in rmarkdown
  # file can then be downloaded and reviewed along with report
  # and moved manually to s3 test/prod
  if (NROW(deltas) > 0) {

    # save flattened version with data to disk
    cop_year_end <- substr(cop_year, 3, 4)
    new_dpm_file_name <- paste0("model_data_pack_input_", cop_year_end, "_", lubridate::today(), "_", commit, "_flat")

    # if run locally with local params set file is automatically saved
    if (isFALSE(posit_server)) {
      saveRDS(flattenDataPackModel_21(cop_data),
               file = paste0(new_dpm_file_name, ".rds"))

    }
  }

  #### CLEANUP ----
  # using paws s3 we get and dump latest datapack model into our file system
  # here we make sure to get rid of it locally

  # Check if the file exists
  if (file.exists(file_name)) {
    # Delete the file
    file.remove(file_name)
    print(paste0(file_name, " deleted successfully from connect server."))
  } else {
    print(paste0(file_name, "File does not exist."))
  }
}

#### END SCRIPT ----------------------------------------------------------------

# models can be downloaded from reports but for development purposes
# and s3 below is some code to help

if (!posit_server) {

#### Send model to s3 and sharepoint ----

cop_year_end <- substr(cop_year, 3, 4)
file_name <- "datapack_model_data.rds"
output_location <- "../" # change to whatever output location you want

# extract commit information if you want to use for write DEV PURPOSES
# during dev this can be tacked onto file name for tracking
commit <-
  devtools::package_info("datapackcommons") %>%
  dplyr::filter(package == "datapackcommons") %>%
  dplyr::pull(source) %>%
  stringr::str_extract(., "(?<=@)\\w{7}")

# write out dpm for s3
readr::write_rds(
  data_new,
  paste0(
    output_location,
    file_name
  ),
  compress = c("gz")
)

# write out dpm for sharepoint - this requires manual upload
readr::write_rds(
  data_new,
  paste0(
    output_location,
    paste0("model_data_pack_input_", cop_year_end, "_", lubridate::today(), "_", commit, "_flat.rds")
  ),
  compress = c("gz")
)

}

# uncomment below to send to s3

# Sys.setenv(
#   AWS_PROFILE = "datapack-testing",
#   AWS_S3_BUCKET = "testing.pepfar.data.datapack"
# )
#
# s3<-paws::s3()
#
# r<-tryCatch({
#   foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
#                      Body = paste0(output_location,"datapack_model_data.rds"),
#                      Key = "support_files/datapack_model_data.rds",)
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
#                      Body = paste0(output_location,"datapack_model_data.rds"),
#                      Key = "support_files/datapack_model_data.rds",)
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
#                    Prefix = "support_files/datapack_model_data.rds") %>%
#   purrr::pluck("Contents", 1, "LastModified")
