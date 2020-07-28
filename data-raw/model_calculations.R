devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)

devtools::install(pkg = "/Users/sam/Documents/GitHub/datapackr",
                  build = TRUE,
                  upgrade = FALSE)

require(datapackcommons)
require(datapackr)
require(tidyverse)
require(jsonlite)
require(lubridate)
require(rlang)
require(assertthat)
require(foreach)

RenameAnalyticsColumns <- function(data){
  data %>% dplyr::rename(!!c("value"="Value", indicator_uid="Data",
                    "org_unit_uid" = "Organisation unit",
                    "period" = "Period")) %>% return()
}

GetData <- function(indicator_parameters, ou_uid, ou_level, ou_name, 
                    dim_item_sets, include_military) {
  indicator_parameters <-  dplyr::as_tibble(indicator_parameters)
  
        analytics_output <- datapackcommons::GetData_DataPack(indicator_parameters,
                                                              ou_uid,
                                                              include_military)
          

        return(dplyr::mutate(indicator_parameters, analytics_output = list(analytics_output)))
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
  }
  else {
    return(list(processed = aggregated_data, was_aggregated = TRUE,
         aggregations = filter(aggregated_data, count>1)))
  }
}

ProcessDataRequiredRow <- function(data_spec, dim_item_sets){
  SelectAndStripPrefix <- function(prefix, df) {
    dplyr::select(df, dplyr::starts_with(prefix, ignore.case = FALSE)) %>%
      dplyr::rename_all(.funs = stringr::str_remove, pattern = prefix)
  }

  assertthat::assert_that(NROW(data_spec) == 1)

  # Return list assumed to start out with a named list of the components to process,
  # no other list elements permissible
  if(is.na(data_spec$calculation)){
    return_list = list(A = NULL)
  } else{
    return_list = list(A = NULL, B = NULL)
  }

  for(component in names(return_list)){
    prefix <- paste0(component, ".")

    component_data <- SelectAndStripPrefix(prefix, data_spec)
    analytics_results <- component_data[[1,"analytics_output"]][[1]]$results
    analytics_api_call <- component_data[[1,"analytics_output"]][[1]]$api_call
    analytics_time <- lubridate::as_datetime(component_data[[1,"analytics_output"]][[1]]$time)

    
    if(is.null(analytics_results)){
      # adding empty version of minimum output columns to cop_data
      return_list[[component]][["processed"]] <-      tibble::tibble(indicator_uid = character(), 
                                                                     org_unit_uid = character(),
                                                                     period = character(),
                                                                     age_option_uid = character(), 
                                                                     sex_option_uid = character(), 
                                                                     kp_option_uid = character(),
                                                                     value = double())
      next
    }
    
      if(NROW(analytics_results) == 0){
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

    component_data <-   AggByAgeSexKpOu2(mapped_data)
    if(component_data$was_aggregated == TRUE){
      assertthat::assert_that(data_spec$allocate == "distribute")
    }

    return_list[[component]] <- c(component_data,
                                  "api_call" = analytics_api_call,
                                  "time" = as.POSIXct(analytics_time,
                                                      origin = "1970-01-01",
                                                      tz = "UTC"))

  }

  if(is.na(data_spec[[1, "calculation"]]) && data_spec[[1, "type"]] == "Numeric"){
    return_list[["results"]] <- return_list[["A"]][["processed"]]
    return(return_list)
  } else if(is.na(data_spec[[1, "calculation"]]) && data_spec[[1, "type"]] == "Percent"){
    return_list[["results"]] <- return_list[["A"]][["processed"]] %>% mutate(value = value/100)
    return(return_list)
  } else{ # performing extra calculation to combine components A and B

# warning suppressed because one_of throws warning if named column is missing
    A <-
      suppressWarnings(dplyr::select(return_list[["A"]][["processed"]],
                                     dplyr::one_of(c("sex_option_uid", "age_option_uid", "kp_option_uid",
                                                     "org_unit_uid", "value")))
                       ) %>%
      dplyr::rename("A"="value")


    B <-  suppressWarnings(dplyr::select(return_list[["B"]][["processed"]],
                                         dplyr::one_of(c("sex_option_uid", "age_option_uid", "kp_option_uid",
                                                         "org_unit_uid", "value")))
                           ) %>%
      dplyr::rename("B"="value")

    joined_data  <-  dplyr::full_join(A, B)

# After join replace NAs with the appropriate value (0 or NA) based on indicator config
    joined_data$A[is.na(joined_data$A)] <- data_spec$A.value_na
    joined_data$B[is.na(joined_data$B)] <- data_spec$B.value_na

    calculated_data <-  data_spec$calculation %>%
      str_replace_all("A", "joined_data$A") %>%
      str_replace_all("B", "joined_data$B") %>%
      rlang::parse_expr() %>%
      eval() %>%  mutate(joined_data, value = .)

    calculated_data$value[is.infinite(calculated_data$value) | is.nan(calculated_data$value)] <- NA

    return_list[["results"]] <- calculated_data
    return(return_list)
  }
}

DHISLogin("/users/sam/.secrets/datim.json")
base_url <- getOption("baseurl")
repo_path <- "/users/sam/Documents/GitHub/COP-19-Target-Setting/"
output_location <- "/Users/sam/COP data/"

 cop_data = list()
# get country and prioritization level
 operating_units <- datapackcommons::GetCountryLevels(base_url) %>%
   dplyr::arrange(country_name) %>% 
  # filter(country_name >= "Tr") %>% 
   dplyr::filter(prioritization_level != 0) # Turkmenistan has no planning/priortization level
 priority_snu_data <- datapackr::getDataValueSets(c("dataElementGroup","period", "orgUnitGroup"),
                                                        c("ofNbyQgD9xG","2019Oct","AVy8gJXym2D")) %>% 
   dplyr::select(org_unit_uid = org_unit, value)
 
# operating_units <- tibble::tribble(
# ~ country_name, ~ id, ~ prioritization_level,
# "Global", "ybg3MO3hcf4", "2")

# get local copy of package config file
dim_item_sets <- datapackcommons::dim_item_sets

# for each ou
for (ou_index in 1:NROW(operating_units)) {
  # start with fresh local copy of data_required.csv
  data_required <-  datapackcommons::data_required
  operating_unit <-  dplyr::slice(operating_units, ou_index)
  cop_data[[operating_unit$id]] = list("ou_name" = operating_unit$country_name,
                                       "ou_psnu_level" = operating_unit$prioritization_level)

  print(operating_unit$country_name)
  print(lubridate::now())

  indicator_parameters <- datapackcommons::StackPrefixedCols(data_required, c("A.", "B.")) %>%
    unique() %>%
    filter(!is.na(dx_id))

  doMC::registerDoMC(cores = 5) # or however many cores you have access to
  include_military <- dplyr::if_else(operating_unit$country_name == "Philippines",
                 FALSE,
                 TRUE)
  analytics_output <- plyr::adply(indicator_parameters, 1, GetData, operating_unit$id, 
                                  operating_unit$prioritization_level,
                                  operating_unit$country_name, 
                                  dim_item_sets,
                                  include_military,
                                  .parallel = TRUE)

# Rename the columns of analytics output with prefix {A, B} to have matching column names in joins
  data_required = analytics_output %>%
  dplyr::rename_all(.funs = function(x) paste0("A.", x)) %>% left_join(data_required, .)
  data_required = analytics_output %>%
    dplyr::rename_all(.funs = function(x) paste0("B.", x)) %>% left_join(data_required, .)

#   for each line in data required
  for (data_required_index in 1:NROW(data_required)){

    data_spec <-  dplyr::slice(data_required, data_required_index)
    cop_data[[operating_unit$id]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]] <-
      ProcessDataRequiredRow(data_spec, dim_item_sets)
  }

# dropping any impatt.priority_snu data that isn't raw data 
  cop_data[[operating_unit$id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.T_1"]][["results"]] <-
    cop_data[[operating_unit$id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.T_1"]][["results"]] %>% 
    dplyr::inner_join(priority_snu_data)
  
# TODO bind rows to create a flat file
  }

print(lubridate::now())
# saveRDS(datapackr::flattenDataPackModel_19(cop_data), file = paste0(output_location,"model_data_pack_input_20_20200727_2_flat.rds"))
# saveRDS(cop_data, file = paste0(output_location,"model_data_pack_input_20_20200727_2.rds"))
# cop_data_new=cop_data

### COMPARISAON CODE FOR TWO DIFFERENT OUTPUT FILES
  # cop_data_old <- readRDS(file = paste0(output_location,"model_data_pack_input_20_20200319_1.rds"))
 #   operating_units <- datapackcommons::GetCountryLevels(base_url)  # %>% filter(country_name >= "Rwanda")
# operating_units <- tibble::tribble(~id, ~country_name,
#                                    "Asia_Regional_Data_Pack","Asia_Regional_Data_Pack",
#                                    "Caribbean_Data_Pack","Caribbean_Data_Pack",
#                                    "Central_America_Data_Pack",  "Central_America_Data_Pack",
#                                    "Western_Africa_Data_Pack", "Western_Africa_Data_Pack")
#
# cop_data_old=cop_data
# deltas = data.frame()
# for (operating_unit in operating_units$id) {
#   print(filter(operating_units, operating_units$id == operating_unit))
#   if (!(operating_unit %in% names(cop_data_old))) {
#     print("country not in original")
#     next
#   }
#   for (data_required_index in 1:NROW(data_required)) {
#     data_spec <-  dplyr::slice(data_required, data_required_index)
#     if (is.null(cop_data_old[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]]) &
#         is.null(cop_data_new[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])) {
#       next
#     }
#     if (is.null(cop_data_old[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]]) &
#         !is.null(cop_data_new[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])) {
#       print("no old but new")
#       print(data_spec)
#       print(cop_data_new[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])
#       next
#     }
#     if (!is.null(cop_data_old[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]]) &
#         is.null(cop_data_new[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])) {
#       print("no new but old")
#       print(data_spec)
#       print(cop_data_old[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])
#       next
#     }
#     old_results = cop_data_old[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]] %>%
#       dplyr::select(dplyr::one_of(
#         c(
#           "sex_option_uid",
#       #    "sex_option_name",
#           "age_option_uid",
#       #    "age_option_name",
#           "kp_option_uid",
#       #    "kp_option_name",
#           "org_unit_uid",
#           "value"
#         )
#       )) %>% dplyr::filter(!is.na(value)) %>% dplyr::mutate(
#         "country_name" = dplyr::filter(operating_units, operating_units$id == operating_unit)[["country_name"]],
#         "country_uid" = operating_unit,
#         "data_pack_sheet" = data_spec$data_pack_sheet,
#         "data_pack_code" = data_spec$data_pack_code
#       )
#     new_results = cop_data_new[[operating_unit]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]] %>%
#       dplyr::select(dplyr::one_of(
#         c(
#           "sex_option_uid",
#       #    "sex_option_name",
#           "age_option_uid",
#       #    "age_option_name",
#           "kp_option_uid",
#       #    "kp_option_name",
#           "org_unit_uid",
#           "value"
#         )
#       )) %>% dplyr::filter(!is.na(value)) %>% dplyr::mutate(
#         "country_name" = dplyr::filter(operating_units, operating_units$id == operating_unit)[["country_name"]],
#         "country_uid" = operating_unit,
#         "data_pack_sheet" = data_spec$data_pack_sheet,
#         "data_pack_code" = data_spec$data_pack_code
#       )
#     verdict <- dplyr::all_equal(old_results, new_results)
# 
#     if (verdict != TRUE) {
#       old_results <- old_results %>% rename(value.old = value)
#       new_results <- new_results %>% rename(value.new = value)
#       print(data_spec$data_pack_code)
#       print(verdict)
#       deltas = full_join(old_results, new_results) %>%
#         filter(value.new != value.old |
#                  is.na(value.new) | is.na(value.old)) %>%
#         dplyr::bind_rows(deltas, .)
#     }
#   }
# }
    #deltas <- deltas %>% dplyr::mutate(org_unit_name =
    #                                 datimvalidation::remapOUs(deltas$org_unit_uid,"ybg3MO3hcf4",mode_in = "id",mode_out = "name"))
