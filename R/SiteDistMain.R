#' @importFrom magrittr %>%
#' @export
#' @title DistributeToSites()
#'
#' @description Based on input parameters - can either 1) return a site density object for a specified country, 
#' 2) apply a provided site density to a provided datapack export, or 3) calculate the density and apply it to
#' a datapack export
#' 
#' Takes data pack export (TargetxPSNUxIM) and several other configuration files and 
#' distributes targets to Site x DSD/TA. The standard COP19 configuration files are included as defaults.
#' Historic data is pulled from the DATIM api and is used to disaggregate 
#' the PSNUxIM level targets. Maping between New targets and historic data in the data_element_map.
#' Data element map references dim_item_sets to know which dissags to use and maps from dimension items to category 
#' options.  
#' If country_name is passed explicitly and d = NULL then this function returns the distribution 
#' densities 
#' @param d Object from data pack export (TargetxPSNUxIM)
#' @param data_element_map Dataframe containing indicator codes for fy20, their details and mapping to historic data
#' @param mechanisms_historic_global Historic mechanism data pulled from datim api to disaggregate PSNUxIM level targets
#' @param dim_tem_sets Dataframe containing all the dimension item sets - standard version is datapackcommons::dim_item_sets
#' @param site_densities An object containing all the site densities of a particular country
#' @param country_name Name of the country used to extract the country levels in a separate object, if parameter d is provided
#' country name is extracted from datapack export object (d$info$datapack_name)
#' @param base_url Specifies the datim instance being used for the api calls, defaults to prod
#' @param verbose If set to true, adds a copy of the densities to output, useful for debugging but can make file large
#' @return An object containing targets distributed to Site x DSD/TA if datapack export was provided,
#' if data pack export is NULL then returns site density object
DistributeToSites <- 
  function(d, 
           data_element_map = datapackcommons::Map19Tto20T, 
           mechanisms_historic_global = 
             datapackcommons::Get19TMechanisms(getOption("baseurl")),
           dim_item_sets = datapackcommons::dim_item_sets,
           site_densities = NULL,
           country_name = NULL, 
           base_url = getOption("baseurl"), 
           verbose = FALSE,
           mech_to_mech_map = NULL,
           sites = NULL){

# get country name from data pack data object unless a country name is provided
  if(is.null(country_name)){
    country_name = d$info$datapack_name
  }
# contains country uids, planning, facility, and community levels    
  country_details <- datapackcommons::GetCountryLevels(base_url, country_name) 
  
# Get the mechanisms relevant for the specifc country being processed
# cache options required for datimvalidation function to work.
# cache age option reverts to original after calling datim validation
  cache_in = getOption("maxCacheAge")
  options(maxCacheAge = 0)
  mechanisms_full_country <- datimvalidation::getMechanismsMap(country_details$id)
  options(maxCacheAge = cache_in)
  
# filter to just those mechanisms with data for the relevant time period
  assertthat::assert_that(assertthat::has_name(mechanisms_historic_global, 
                                               "categoryOptionComboId"),
                          assertthat::has_name(mechanisms_full_country, "id"))
  mechanisms_historic_country <- mechanisms_historic_global %>%   
    dplyr::filter(categoryOptionComboId %in% mechanisms_full_country$id)

# alply to call SiteDensity for each row of data_element_map (each target data element)
# will have a historic distribution for each target, DSD/TA, and site given psnu/IM
# alply uses parallel processing here 
  
  if (is.null(site_densities)) {
    site_densities <- CalculateSiteDensities(data_element_map, country_details, 
                                             mechanisms_historic_country, 
                                             dim_item_sets, base_url, cores = 5)
  }

# if user passed a country name and a null datapack export object 
# then we return the historic site density
  if(is.null(d)){
    return(site_densities)
  }
  
  if(verbose == TRUE){ # add a copy of the densities to output, useful for debugging but can make file large
    d[["data"]][["site"]][["densities_original"]] <- site_densities
  }

# grab the datapack export data from the main data object
# store column names
  datapack_data  <-  d[["data"]][["distributedMER"]] 
  names_in = names(datapack_data)
  
# add copies of Age, Sex, KeyPop columns from data pack
# with columns named as in site densities age_option_name, sex_option_name, kp_option_name
  datapack_data  <- datapack_data %>% dplyr::mutate(age_option_name = Age, 
                                             sex_option_name = Sex,
                                             kp_option_name = KeyPop) 
# modify some age_option_names in input data to match names in site densities
# do this in the newly created columns leaving originals as is
  datapack_data$age_option_name[datapack_data$age_option_name == "<01"] <- "<1" 
  datapack_data$age_option_name[datapack_data$age_option_name == "01-04"] <- "1-4"
  datapack_data$age_option_name[datapack_data$age_option_name == "05-09"] <- "5-9"
  datapack_data$age_option_name[datapack_data$age_option_name == "<= 02 months"] <- "<= 2 months"
  datapack_data$age_option_name[datapack_data$age_option_name == "02 - 12 months"] <- "2 - 12 months"


if(!(is.null(mech_to_mech_map) && is.null(sites))){
  site_densities <- AdjustSiteDensity(site_densities, mech_to_mech_map, sites)
  
  if(verbose == TRUE){ # add a copy of the densities to output, useful for debugging but can make file large
    d[["data"]][["site"]][["densities_adjusted"]] <- site_densities
  }  
  
}


  
# do individual joins for each data element 
# The site densities contain different columns depending on the data elements 
# e.g. sometime age and sex columns other time just key population columns
# so doing each inner join seperatly makes sure the join occurs only on the columns
# in site densities (in common with the data pack data) 
# After the joins bind the results of each individual join into one large data frame
  matched_data <-  purrr::map(site_densities, dplyr::inner_join, datapack_data) %>% 
    purrr::reduce(dplyr::bind_rows)

# Now join the historic data back to the original input data so we have both the data matched to historic data
# and the unmatched data which is still only at the PSNU level.
# Where applicable give the distributed site value by multiplying PSNU level target
# by Site x DSD/TA density
# recode the dsd/ta dimension item ids to names
  
  site_tool_data <- dplyr::left_join(datapack_data, matched_data) %>%  
    dplyr::mutate(siteValue = percent * value)
  site_tool_data$`Support Type`[site_tool_data$`Support Type` == "cRAGKdWIDn4"] <- "TA" 
  site_tool_data$`Support Type`[site_tool_data$`Support Type` == "iM13vdNLWKb"] <- "DSD"
  # Select the columns of interest for the site tool generation process.      
  columns <- c(names_in, "org_unit", "type", "percent", "siteValue", "siteValueH", "psnuValueH")
  site_tool_data <- site_tool_data %>% 
    dplyr::rename("type" = "Support Type", "org_unit" = "Organisation unit") %>% 
    dplyr::select(dplyr::one_of(columns))
  d[["data"]][["site"]][["distributed"]] <- site_tool_data

  return(d)
  }

#' @title CalculateSiteDensities(data_element_map, country_details, mechanisms, dim_item_sets, base_url, cores)
#' 
#' @description Wrapper function to functionalize and parallelize calls to CalculateSiteDensity
#' @param data_element_map Dataframe to be processed with CalculateSiteDensity
#' @param country_details Country levels for a country
#' @param mechanisms All historic mechanisms for the country filtered by id
#' @param dim_item_sets Dataframe containing all the dimension item sets
#' @param base_url Refers to instance of datim being accessed
#' @param cores Number of cores to use for parallel execution
#' @return  Data containing all site densities for the parameters passed
CalculateSiteDensities <- function(data_element_map, country_details, 
                                   mechanisms, dim_item_sets, base_url, cores = 1){
  # alply to call SiteDensity for each row of data_element_map (each target data element)
  # will have a historic distribution for each target, DSD/TA, and site given psnu/IM
  # alply uses parallel processing here 
  
  doMC::registerDoMC(cores = cores) 
  site_densities <- plyr::alply(data_element_map, 1, CalculateSiteDensity, 
                                country_details, mechanisms, 
                                dim_item_sets, base_url, 
                                .parallel = TRUE)
  stats::setNames(site_densities, data_element_map$indicatorCode_fy20_cop)
}
  
#' @title CalculateSiteDensity(data_element_map_item, country_details, mechanisms, dim_item_sets, base_url)
#' 
#' @description Calculates historic distribution for each target, DSD/TA, and site given psnu/IM
#' @param data_element_map_item Each row of data_element_map being sliced and passed
#' @param country_details Country levels for a country
#' @param mechanisms All historic mechanisms for the country filtered by id
#' @param dim_item_sets Dataframe containing all the dimension item sets
#' @param base_url Refers to instance of datim being accessed
#' @return  Dataframe containing site density for the parameters passed
CalculateSiteDensity <- function(data_element_map_item, country_details, 
                                 mechanisms, dim_item_sets, base_url){
  assertthat::assert_that(NROW(data_element_map_item) == 1,
                          NROW(country_details) == 1)
  
# get list of dimensions (parameters) for analytics call by level {planning, community, facility}
# analytics will be called seperatly for each level  
  dimensions_by_ou_level <- BuildDimensionLists(data_element_map_item, dim_item_sets, 
                                                mechanisms, country_details)

# Grab historical data by level ierating over the list just created
  analytics_output_list <-  purrr::map(dimensions_by_ou_level, 
                                       datapackcommons::GetData_Analytics, 
                                       base_url)
  
  if(NROW(analytics_output_list$planning$results) == 0){
    return(stats::na.omit(tibble::tribble(~indicatorCode,
                           NA_character_))) # to do return something more useful?
  }  

  # Do I need to be concerned about rounding error
  
  check_sum_1p = (sum(analytics_output_list$planning$results$Value))
  check_sum_1s = (sum(analytics_output_list$facility$results$Value) +
                    sum(analytics_output_list$community$results$Value))
  
  assertthat::assert_that(check_sum_1p == check_sum_1s)
    
  planning_data <- 
    TransformAnalyticsOutput_SiteTool(analytics_output_list$planning$results, 
                                      dim_item_sets, data_element_map_item,
                                      country_details$planning_level) %>% 
    .[["processed"]] %>% 
    dplyr::select(-dplyr::one_of("Organisation unit", "count", "maximum", "minimum")) %>% 
    dplyr::rename("psnuValueH" = "Value")
  
  site_data <- dplyr::bind_rows(analytics_output_list$community$results,
                                analytics_output_list$facility$results) %>% 
    TransformAnalyticsOutput_SiteTool(dim_item_sets, data_element_map_item,
                                      country_details$planning_level) %>% 
    .[["processed"]] %>% 
    dplyr::select(-dplyr::one_of("count", "maximum", "minimum")) %>% 
    dplyr::rename("siteValueH" = "Value")
  
  joined_data <- dplyr::left_join(site_data, planning_data) %>%
    dplyr::left_join(mechanisms, by = c("Funding Mechanism" = "categoryOptionId")) %>% 
    dplyr::rename("mechanismCode" = "code")  %>% 
    dplyr::select(-name, -categoryOptionComboId) %>% 
    dplyr::mutate(percent = siteValueH/psnuValueH, 
                  indicatorCode = data_element_map_item$indicatorCode_fy20_cop)
  
  check_sum_2p = planning_data$psnuValueH %>% sum()
  assertthat::assert_that(check_sum_1p == check_sum_2p)
  
  check_sum_2s = site_data$siteValueH %>% sum()
  assertthat::assert_that(check_sum_1p == check_sum_2s)

  check_sum_3p  <- joined_data %>% dplyr::select(-dplyr::one_of("percent", "siteValueH", 
                                          "Organisation unit", "Support Type",
                                          "Type of organisational unit")) %>% 
                                            unique() %>% 
                                            .[["psnuValueH"]] %>% 
                                            sum()
  assertthat::assert_that(check_sum_3p == check_sum_1p)
  check_sum_3s <-  joined_data$siteValueH %>% sum()
#  return(check_sum_3p - check_sum_3s)
 
  assertthat::assert_that(check_sum_3s == check_sum_1p)
# return(paste(check_sum_1p,
#              check_sum_1s,
#             check_sum_2p,
#              check_sum_2s,
#              check_sum_3s,
#              check_sum_3p))
  return(joined_data)
} 

#' @title AggBySexKpOuMechSt(data)
#' 
#' @description Calculates the aggregated data and returns it depending on the options present
#' @param data Input data which is read and processed to calculate the aggregate value
#' @return  Aggregated data for the input with summarized Value
AggByAgeSexKpOuMechSt <- function(data) {
  #to do add assertions must include value and org unit columns
  aggregated_data <- data %>%
    dplyr::select(dplyr::one_of(c("sex_option_uid", "sex_option_name",
                              "age_option_uid", "age_option_name",
                              "kp_option_uid", "kp_option_name",
                              "Organisation unit", 
                              "Funding Mechanism", "Support Type",
                              "psnuid", "Value"))) %>%
    dplyr::group_by_at(dplyr::vars(-Value)) %>% 
    dplyr::summarise(count = dplyr::n(), minimum = min(Value), 
              maximum = max(Value), Value = sum(Value)) %>% 
    dplyr::ungroup()
  
  if (max(aggregated_data$count) == 1) { # nothing aggregated
    return(list(processed = data, was_aggregated = FALSE))
  } 
  else {
    return(list(processed = aggregated_data, was_aggregated = TRUE, 
                aggregations = dplyr::filter(aggregated_data, count > 1)))
  }
}

#' @title BuildDimensionLists(data_element_map_item, dim_item_sets, mechanisms, country_details)
#' 
#' @description # get list of dimensions (parameters) for analytics call by level {planning, community, facility}
#' analytics will be called seperatly for each level.
#' Tightly coupled to DATIM as it contains some hard coded dimension uids for Funding Mechanism, technical area, 
#' num or denom, disagg type, support type, and type of organization unit. Also some hard coded dimension items for
#' type of organization unit and support type
#' @param data_element_map_item Single row of data_element_map being sliced and passed
#' @param dim_item_sets Dataframe containing all the dimension item sets e.g. datapackcommons::dim_item_sets
#' @param mechanisms All historic mechanisms for the country filtered by id
#' @param country_details Country levels for a country
#' @return  List of dimensions for the analytics call GetData_Analytics
BuildDimensionLists <- function(data_element_map_item, dim_item_sets, 
                                mechanisms, country_details){
  # prepare df of dimensions and filters as expected by GetData_analytics  
  dimension_disaggs <- dim_item_sets %>% dplyr::mutate(type = "dimension") %>%  
    dplyr::filter(model_sets %in% c(data_element_map_item$age_set,
                                    data_element_map_item$sex_set,
                                    data_element_map_item$kp_set,
                                    data_element_map_item$other_disagg)) %>% 
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>% 
    stats::na.omit() # there are some items in dim item sets with no source dimension
  
  dimension_mechanisms <- mechanisms["categoryOptionId"] %>% dplyr::transmute(type = "dimension", 
                                                                              dim_item_uid = categoryOptionId,
                                                                              dim_uid = "SH885jaRe0o")
  
  # dimensions used by both numerator and denominator and prepped for GetData_Analytics
  dimensions_common <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "filter", data_element_map_item[[1,"dx"]],"dx", 
                    "filter", data_element_map_item[[1,"pe"]], "pe",
                    "dimension", country_details$id, "ou",
                    "dimension", data_element_map_item[[1,"technical_area_uid"]], "LxhLO68FcXm",
                    "dimension", data_element_map_item[[1,"num_or_den_uid"]],"lD2x0c8kywj",
                    "dimension", data_element_map_item[[1,"disagg_type_uid"]],"HWPJnUTMjEq") %>% 
    dplyr::bind_rows(dimension_mechanisms, dimension_disaggs)
  
  dimensions_planning <- #filter on community and facility gets rid of military PSNU
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "dimension", paste0("LEVEL-", country_details$planning_level), "ou",
                    "filter", "POHZmzofoVx", "mINJi7rR1a6", # facility type of organization unit
                    "filter", "PvuaP6YALSA","mINJi7rR1a6" # community type of organization unit
    ) %>% 
    dplyr::bind_rows(dimensions_common)
  
  dimensions_facility <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "dimension", paste0("LEVEL-", country_details$facility_level), "ou",
                    "dimension", "POHZmzofoVx", "mINJi7rR1a6", # facility org type
                    "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                    "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
    dplyr::bind_rows(dimensions_common)
  
  dimensions_community <- 
    tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                    "dimension", paste0("LEVEL-", country_details$community_level), "ou",
                    "dimension", "PvuaP6YALSA","mINJi7rR1a6", # community org type
                    "dimension", "iM13vdNLWKb", "TWXpUVE2MqL", #dsd and ta support types
                    "dimension", "cRAGKdWIDn4", "TWXpUVE2MqL") %>% 
    dplyr::bind_rows(dimensions_common)
  
  list(planning = dimensions_planning,
       facility = dimensions_facility,
       community = dimensions_community)
}

## Draft perhaps temporary Function
#' @title CheckSiteToolData(d, d_old)
#' 
#' @description Performs some checks on the results of distributing data to sites
#' @param d list - the output of DistributeToSites
#' @param d_old list - a prior output of DistributeToSites
#' if present check includes comparison
#' all_equal(d_new$data$site$distributed, d_old$data$site$distributed) 
#' @param issue_error boolean - if true failing one or more tests 
#' will return an error
#' @return  list with various check outcomes
CheckSiteToolData <- function(d, d_old = NULL, issue_error = FALSE){
  
  datapack_data  <-  d[["data"]][["distributedMER"]]
  site_tool_data  <-  d[["data"]][["site"]][["distributed"]]
  site_tool_data_site <- dplyr::filter(site_tool_data, !is.na(siteValue))
  site_tool_data_psnu_only <- dplyr::filter(site_tool_data, is.na(siteValue))
# sum of the value column in the input data by indicator code
  sum_dp = datapack_data %>% dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(original = sum(value, na.rm=TRUE)) %>% dplyr::ungroup()

# sum of data left at the psnu level (nothing to distribute to) determined by siteValue = NA
  sum_site_psnu_only  <-  
    site_tool_data_psnu_only %>% 
    dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(psnu = sum(value, na.rm=TRUE)) %>% 
    dplyr::ungroup()
  
  totals <-  
    site_tool_data %>% 
    dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(site = sum(siteValue, na.rm=TRUE)) %>% 
    dplyr::ungroup() %>% dplyr::full_join(sum_site_psnu_only) %>% 
    dplyr::full_join(sum_dp) %>% dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(input_total = sum(input = sum(original, na.rm=TRUE)), 
                     site_tool_total = sum(site, na.rm=TRUE) + sum(psnu, na.rm=TRUE),
                     difference = input_total - site_tool_total) %>% 
    dplyr::ungroup()
  print(totals)
  mechanisms_not_distributed <- dplyr::setdiff(datapack_data$mechanismCode, 
                                             site_tool_data_site$mechanismCode)
  print(mechanisms_not_distributed)
  psnus_not_distributed <- dplyr::setdiff(datapack_data$psnuid, 
                                               site_tool_data_site$psnuid)
  print(psnus_not_distributed)
  targets_not_distributed <- dplyr::setdiff(datapack_data$indicatorCode, 
                                          site_tool_data_site$indicatorCode)    
  print(targets_not_distributed)
  
  indicators_ages_dp <- datapack_data %>% dplyr::select(indicatorCode, Age) %>% unique()
  indicators_ages_site <- site_tool_data_site %>% dplyr::select(indicatorCode, Age) %>% unique()
  indicator_age_combos_not_distributed = dplyr::anti_join(indicators_ages_dp, indicators_ages_site)
  
  indicators_sexes_dp <- datapack_data %>% dplyr::select(indicatorCode, Sex) %>% unique()
  indicators_sexes_site <- site_tool_data_site %>% dplyr::select(indicatorCode, Sex) %>% unique()
  indicator_sexes_combos_not_distributed = dplyr::anti_join(indicators_sexes_dp, indicators_sexes_site)
  
  indicators_kps_dp <- datapack_data %>% dplyr::select(indicatorCode, KeyPop) %>% unique()
  indicators_kps_site <- site_tool_data_site %>% dplyr::select(indicatorCode, KeyPop) %>% unique()
  indicator_kps_combos_not_distributed = dplyr::anti_join(indicators_kps_dp, indicators_kps_site)
  
  # make sure all the historic disaggs map to a disagg in the datapack data
  # ages_density_data <- purrr::map(site_densities, "age_option_name") %>% purrr::reduce(dplyr::union)
  # sexes_density_data <- purrr::map(site_densities, "sex_option_name") %>% purrr::reduce(dplyr::union)
  # kps_density_data <- purrr::map(site_densities, "kp_option_name") %>% purrr::reduce(dplyr::union)
  # print( unique(datapack_data$kp_option_name))
  # assertthat::assert_that(setequal(ages_density_data, datapack_data$age_option_name),
  #                         setequal(sexes_density_data, datapack_data$sex_option_name),
  #                         setequal(kps_density_data, datapack_data$kp_option_name)
  #                         )
   

  # save the original total by indicator of incoming targets for checking later

  # sanity check that the sum of targets in the input 
  #    equals the sum of targets in the output file
  

  
  #assertthat::assert_that(sum_in == sum_out_psnu + sum_out_site)
  
  d[["data"]][["site"]][["other"]] <- sum_reconcile
  
  
  
  # left <<- dplyr::left_join(site_tool_data, matched_data)
  # inner <<- dplyr::inner_join(site_tool_data, matched_data)
  # anti_dp <<- dplyr::anti_join(site_tool_data, matched_data)
  # anti_h <<- dplyr::anti_join(matched_data, site_tool_data)
  
  
  # setdiff(site_tool_data$psnuid, matched_data$psnuid)
  # setdiff(site_tool_data$mechanismCode, matched_data$mechanismCode)
  # setdiff(site_tool_data$indicatorCode, matched_data$indicatorCode)
  # setdiff(site_tool_data$age_option_name, matched_data$age_option_name)
  
  # eswitani reconciliation
  #     HTS_INDEX_COM.N.Age/Sex/Result.20T.NewPos
  # reconcile <- anti_dp %>% 
  #   filter(psnuid != "WtniIbuJk7k",
  #          !(indicatorCode %in% setdiff(datapack_data$indicatorCode, historic_data$indicatorCode)),
  #          (indicatorCode != "HTS_INDEX_COM.N.Age/Sex/Result.20T.NewNeg"), #eswatini
  #          (indicatorCode != "HTS_INDEX_COM.N.Age/Sex/Result.20T.NewPos"),#eswatini
  #          Age != "<15", Age != "15+",
  #          !(mechanismCode %in% setdiff(datapack_data$mechanismCode, historic_data$mechanismCode)) )
  # 
  # drill <- matched_data %>% filter(#psnuid=="nxGb6sd7p7D", 
  #                                   indicatorCode == "PMTCT_STAT.D.Age/Sex.20T")#,
  #                                   #mechanismCode=="18599")
  
  # dplyr::all_equal(d_new$data$site$distributed, d_old$data$site$distributed) 
}

#' @title TransformAnalyticsOutput_SiteTool()
#' 
#' @description Transforms API results into a table ready for downstream processing.
#' Takes API analytics output (which is disaggrepgated by dimensions 
#' and dimension items) and maps to cateogry options needed in output. This
#' includes splitting or aggregating dimension items related to age, sex, kp, and other_disagge 
#' as specified in config files, dim_item_sets and data element map.
#' 
#' Additionally creates a psnuid column from the ou_hierarchy column and drops
#' the ou_hierarchy column.
#' @param analytics_results - data frame - this is a results list item returned from
#' calling GetData_Analytics and corresponds to the data element map item  
#' @param dim_item_sets Dataframe containing all the dimension item sets 
#' e.g. datapackcommons::dim_item_sets
#' @param data_element_map_item Single row of data_element_map being sliced and passed
#' @param planning_level the planning level to use when pulling psnuid from 
#' analytics_results$ou_hierarchy
TransformAnalyticsOutput_SiteTool <- function(analytics_results, dim_item_sets, 
                                          data_element_map_item, planning_level){

# Create a list of data frames each with the specific dimension item category 
# option combinations specified in data element map 
  disagg_sets  <-  c("age_set", 
                     "sex_set", 
                     "kp_set", 
                     "other_disagg") %>% 
    purrr::map(~dplyr::filter(dim_item_sets,
                              model_sets == data_element_map_item[[1, .]]))
  
  purrr::reduce(disagg_sets, 
                MapDimToOptions, 
                allocate = "distribute", 
                .init = analytics_results) %>%  
    dplyr::mutate(psnuid = purrr::map_chr(.$ou_hierarchy, planning_level)) %>% 
    dplyr::select(-ou_hierarchy) %>% 
    AggByAgeSexKpOuMechSt()
  
  ### MEchanism to Mechanism mapping would happen right here.
  ### The config fle would look something like this
  
  ### ~technical area, , ~psnu, ~old_mechanism_uid or code, ~new_mechanism_uid or code, ~weight
}

# this takes a stored density and alters it based on mechanism to mechanism mapping and 
# dropping of some sites
AdjustSiteDensity <- function(site_densities, mech_to_mech_map = NULL, sites = NULL){

# I run this even if sites is null because it adds some expected columns even in the null case
  site_densities_post_site_drop = purrr::map(site_densities, DropSitesFromDensity, sites)

  purrr::map(site_densities_post_site_drop, MapMechToMech, mech_to_mech_map)
  
}

#' @title DropSitesFromDensity
#'
#' @description Drops sites from a density data frame if they are not part of a list in sites,
#' recalculates psnuValueH and percent (density) based on dropping the sites 
#' @param site_density dataframe from density object
#' @param sites character vector - list of site uids to KEEP 
#' @return Returns original density with two additional columns: dropped_site_reduction and
#' psnuValueH_after_site_drop. Additionally recalculated the percent column if sites are 
#' dropped. 
DropSitesFromDensity <- function(site_density, sites = NULL) {

# check to see if we have any historic data for this data element
  if(NROW(site_density) == 0){
    return(site_density)
  }

  # if no sites to drop (meaning no explicit site list provided) 
  # add some columns expected down stream to site_density and return
  if (is.null(sites)) {
    return(
      site_density %>%
        mutate(
          dropped_site_reduction = 0,
          psnuValueH_after_site_drop = psnuValueH
        )
    )
  }
  
  site_data_to_drop <- site_density %>%
    dplyr::filter(!(site_density$`Organisation unit` %in% sites)) %>%
    dplyr::select(-percent) # we need to recalculate percent so drop it here
  
# we are keeping all the data as is
# add some columns expected down stream to site_density and return
  if (NROW(site_data_to_drop) == 0) {
    return(
      site_density %>%
        mutate(
          dropped_site_reduction = 0,
          psnuValueH_after_site_drop = psnuValueH
        )
    )
  }
  
  site_data_to_keep <- site_density %>%
    dplyr::filter((site_density$`Organisation unit` %in% sites)) %>%
    select(-percent) # we need to recalculate percent so drop it here

## TODO change to work like mech to mech mapping? - get psnu total by summing the 
## site total instead of reducing the psnu value originally pulled via API
  
  # get the IM x PSNU adjustment, group by excludes all columns that are
  # site specific since we want a sum of siteValueH for dropped sites by psnu x im
  psnu_reductions =  site_data_to_drop %>%
    dplyr::group_by_at(dplyr::vars(
      -siteValueH,
      -psnuValueH,-`Organisation unit`,
      -`Support Type`
    )) %>%
    dplyr::summarise(dropped_site_reduction = sum(siteValueH)) %>%
    dplyr::ungroup()
  
  # psnu_reductions retained all variables common to all sites + disaggs and dropped the site
  # specific variables, so we can use left join to attach the new column and perform the
  # new density calculation
  new_site_density <- site_data_to_keep %>%
    left_join(psnu_reductions) %>%
    mutate(dropped_site_reduction = tidyr::replace_na(dropped_site_reduction, 0)) %>%
    mutate(
      psnuValueH_after_site_drop = psnuValueH - dropped_site_reduction,
      percent = siteValueH / (psnuValueH - dropped_site_reduction)
    )
  
  # sum(new_site_density$percent)
  # site_data_to_keep %>% dplyr::select(-dplyr::one_of("percent", "siteValueH",
  #                                                    "Organisation unit", "Support Type",
  #                                                    "Type of organisational unit")) %>%
  #   unique() %>%
  #   .[["psnuValueH"]] %>%
  #   sum()
  #
  # new_site_density %>% dplyr::select(-dplyr::one_of("percent", "siteValueH",
  #                                                "Organisation unit", "Support Type",
  #                                                "Type of organisational unit")) %>%
  #   unique() %>%
  #   .[["psnuValueH_after_site_drop"]] %>%
  #   sum()
  return(new_site_density)
}

MapMechToMech <- function(site_density, mech_to_mech_map_full = NULL){
 # a testing mech to mech map
  mech_to_mech_map_full <-
    tibble::tribble(~psnuid, ~`Technical Area`, ~`Numerator / Denominator`,
                    ~`Support Type`, ~oldMech, ~newMech, ~percent,
                    "nxGb6sd7p7D", "PMTCT_STAT", "D", "DSD", "17460", "70270", .7,
                    "nxGb6sd7p7D", "PMTCT_STAT", "D", "DSD", "17460", "70271", .3,
                    "nxGb6sd7p7D", "PMTCT_STAT", "D", "DSD", "18599", "70270", .5,
                    "nxGb6sd7p7D", "PMTCT_STAT", "D", "DSD", "18599", "70271", .5,
                    "nxGb6sd7p7D", "OVC_SERV", "N", "DSD", "18599", "70271", 1,
                    "nxGb6sd7p7D", NA, "N", "DSD", "18599", "70271", 1)
  
  if(is.null(mech_to_mech_map_full)){
    return(site_density)
    #TODO make sure I don't need to add any columns    
  }
  
  mech_to_mech_map_full$`Support Type`[mech_to_mech_map_full$`Support Type` == "TA"] <- "cRAGKdWIDn4" 
  mech_to_mech_map_full$`Support Type`[mech_to_mech_map_full$`Support Type` == "DSD"] <- "iM13vdNLWKb" 

  if(NROW(site_density) == 0){
    return(site_density)
  }
  
  technical_area = site_density[[1, "indicatorCode"]]  %>% 
    stringr::str_split("\\.") %>% .[[1]] %>% .[[1]]
  num_or_den = site_density[[1, "indicatorCode"]]  %>% 
    stringr::str_split("\\.") %>% .[[1]] %>% .[[2]]
  

# TODO Sid - for ethiopia here we would also need to retain rows of mechanism map 
# where technical area is null or NA  
  mech_to_mech_map <- mech_to_mech_map_full %>% 
    dplyr::filter(`Technical Area` == technical_area | `Technical Area` == NA,
                  `Numerator / Denominator` == num_or_den) %>% 
    dplyr::select(-`Technical Area`, -`Numerator / Denominator`)
  
  
# if support type in mech to mech map is null add a row for 
# DSD and a row for TA 


  # see if any mapping required after filtering to relevant rows of mech_to_mech_map
  if(NROW(mech_to_mech_map) == 0){
    return(site_density)
    #TODO make sure I don't need to add any columns
  }
  
# see if there are rows to be recoded, if not throw an error since we really shouldn't
# receive a mapping that has elements which do not match data 
  # if(!any(mech_to_mech_map$`Support Type` == site_density$`Support Type` & 
  #    mech_to_mech_map$psnuid == site_density$psnuid &
  #    mech_to_mech_map$oldMech == site_density$mechanismCode)){
  #   stop(paste("Mechanism map in MapMechToMech has entries with no matching data.",
  #               technical_area, num_or_den, 
  #       utils::str(mech_to_mech_map)))
  # }
  
  site_density_new  <-  site_density %>% dplyr::rename("oldMech" = "mechanismCode")

  site_density_new  <- dplyr::left_join(site_density_new, mech_to_mech_map) %>% 
    dplyr::mutate(mechanismCode = dplyr::if_else(is.na(newMech), oldMech, newMech)) %>% 
    dplyr::mutate(weight = tidyr::replace_na(weight, 1)) %>% 
    dplyr::mutate(siteValueH_adjusted = siteValueH * weight) %>% 
    dplyr::group_by_at(dplyr::vars(-`Funding Mechanism`, -psnuValueH, -oldMech, -psnuValueH_after_site_drop, 
                                   -percent, -weight, -siteValueH_adjusted)) %>%
    dplyr::summarise(siteValueH_adjusted = sum(siteValueH_adjusted)) %>% dplyr::ungroup()
    
    #TODO figure out how to include kp_option_name
    psnu_new  <-  
      suppressWarnings(dplyr::select(site_density_new,
                                     dplyr::one_of("psnuid", "mechanismCode",
                                                   "age_option_name", "sex_option_name", 
                                                   "kp_option_name", "siteValueH_adjusted"))) %>% 
      dplyr::group_by_at(dplyr::vars(-siteValueH_adjusted)) %>%
      dplyr::summarise(psnu_new = sum(siteValueH_adjusted))
      
  site_density_new <- dplyr::left_join(site_density_new, psnu_new) %>% 
    dplyr::mutate(percent = siteValueH_adjusted/psnu_new)
  
# temp =  site_density_new %>% dplyr::group_by_at(dplyr::vars(age_option_name, sex_option_name, psnuid, mechanismCode)) %>%
#   dplyr::summarise(sum=sum(percent))
  
  
#   site_density_new %>% dplyr::select("psnuid", "age_option_name", "sex_option_name", 
#                                      "mechanismCode","psnuValueH_after_site_drop") %>%
#     unique() %>%
#     .[["psnuValueH_after_site_drop"]] %>%
#     sum()
#   
# site_density_new %>% dplyr::select("psnuid", "age_option_name", "sex_option_name", 
#                                      "mechanismCode","psnuValueH_adjusted") %>%
#     unique() %>%
#     .[["psnuValueH_adjusted"]] %>%
#     sum()
# 
# 
# temp = site_density_new %>% dplyr::group_by_at (dplyr::vars("psnuid", "oldMech","age_option_name", "sex_option_name", 
#                                    "mechanismCode")) %>%
#   dplyr::summarise(count = dplyr::n(),min = min(psnuValueH_adjusted), max = 
#                      max(psnuValueH_adjusted)) %>% ungroup() %>% .["psnuValueH_adjusted"] %>% 
#   sum()
# 
### looks like I am not getting back the psnu total because I am getting a DSD and a TA line for some of these due to the 
### spklite to new mech only affecting DSDD (in my example)
  


  # sum(new_site_density$percent)
  # site_data_to_keep %>% dplyr::select(-dplyr::one_of("percent", "siteValueH",
  #                                                    "Organisation unit", "Support Type",
  #                                                    "Type of organisational unit")) %>%
  #   unique() %>%
  #   .[["psnuValueH"]] %>%
  #   sum()
  #

  # temp = site_density_new %>% dplyr::select("psnuid", "age_option_name", "sex_option_name", 
  #                                            "mechanismCode","psnuValueH_adjusted") %>%
  #   unique() %>%
  #   .[["psnuValueH_adjusted"]] %>%
  #   sum()
  # 
  # 
  #   temp = site_density_new %>% dplyr::select(-dplyr::one_of("percent", "siteValueH", "psnuValueH",
  #                                                   "oldMech", "dropped_site_reduction",
  #                                                   "psnuValueH_after_site_drop", "siteValueH_adjusted", 
  #                                                "Organisation unit", "Support Type",
  #                                                "Type of organisational unit", "weight",
  #                                                "newMech", "Funding Mechanism")) %>%
  #   unique() %>%
  #   .[["psnuValueH_adjusted"]] %>%
  #   sum()
  
  return(site_density_new)
  }
