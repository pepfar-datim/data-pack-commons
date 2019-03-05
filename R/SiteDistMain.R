main <- function(){
  devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                    build = TRUE,
                    upgrade = FALSE)
  
   require(plyr)
   require(datapackcommons)
   require(tidyverse)
   require(jsonlite)
   require(assertthat)
   require(foreach)
   require(datimvalidation)
  
  datapackcommons::DHISLogin("/users/sam/.secrets/triage.json")
  base_url <- getOption("baseurl")
  # sample input file from sharepoint
  
 d =  readr::read_rds("/Users/sam/Desktop/site tool samples/Results Archive_Eswatini_20190304170332.rds")
# d =  readr::read_rds("/Users/sam/Desktop/site tool samples/Results Archive_Nigeria_20190304170241.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/Ethiopia_Results_Archive20190214204719.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/Malawi_Results_Archive20190214165548.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/Mozambique_Results_Archive20190215144113.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/Namibia_Results_Archive20190215163433.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/South Africa_Results_Archive20190215143802.rds")
# d =   readr::read_rds("/Users/sam/Desktop/site tool samples/West-Central Africa Region_Results_Archive20190222135620.rds")
# d =    readr::read_rds("/Users/sam/Desktop/site tool samples/Zambia_Results_Archive20190214200342.rds")
# d =   readr::read_rds("/Users/sam/Desktop/site tool samples/Zimbabwe_Results_Archive20190214184527.rds")
 # d_old <- readr::read_rds("/Users/sam/Desktop/site tool samples/Results Archive_Eswatini_20190304170332_site.rds")
 
  # mechanisms with data for FY19. calling this places a lock on data value table in datim, 
  # so run as little as possible. Run once here and pass as argument toDistributeToSites 
   if(!exists("mechanisms_19T")){
     mechanisms_19T <<- datapackcommons::Get19TMechanisms(base_url)
   }
 
   #country_name <- "Rwanda"
# if passing a country level data pack to DistributeToSites 
# without passing country_name parameter then the country will be pulled from main data object
# d$info$datapack_name
  
 DistributeToSites(d,
                   #, data_element_map = 
                   #  datapackcommons::Map19Tto20T # %>% filter(stringr::str_detect(technical_area,"PMTCT")) 
                   , mechanisms_historic_global = mechanisms_19T
                   #, datapackcommons::dim_item_sets,
                   #, country_name = "Tanzania", 
                   #, base_url = base_url
                    , verbose = TRUE)
  }

#' @importFrom dplyr %>%
#' @title DistributeToSites(d, 
#' data_element_map = datapackcommons::Map19Tto20T, 
#' mechanisms_historic_global = datapackcommons::Get19TMechanisms("https://www.datim.org/"),
#' dim_item_sets = datapackcommons::dim_item_sets,
#' country_name = NULL, base_url = getOption("baseurl"), verbose = FALSE)
#'
#' @description Takes data pack export (TargetxPSNUxIM) and several other configuration files and 
#' distributes targets to Site x DSD/TA. Historic data is pulled from the DATIM api and is used to disaggregate 
#' the PSNUxIM level targets. Maping between New targets and historic data in the data_element_map.
#' Data element map references dim_item_sets to know which dissags to use and maps from dimension items to category 
#' options.  
#' If country_name is passed explicitly and d = NULL then this function returns the distribution 
#' densities 
DistributeToSites <- 
  function(d, 
           data_element_map = datapackcommons::Map19Tto20T, 
           mechanisms_historic_global = 
             datapackcommons::Get19TMechanisms("https://www.datim.org/"),
           dim_item_sets = datapackcommons::dim_item_sets,
           country_name = NULL, 
           base_url = getOption("baseurl"), 
           verbose = FALSE){

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
  
  site_densities <- CalculateSiteDensities(data_element_map, country_details, 
                                           mechanisms_historic_country, 
                                           dim_item_sets, base_url, cores = 5)

# if user passed a country name and a null datapack export object 
# then we return the historic site density
  if(is.null(d)){
    return(site_densities)
  }
  
  if(verbose == TRUE){ # add a copy of the denities to output, useful for debugging but can make file large
    d[["data"]][["site"]][["densities"]] <- site_densities
  }

# grab the datapack export data from the main data object
# store column names
  datapack_data  <-  d[["data"]][["distributedMER"]] 
  names_in = names(datapack_data)
  
# add copies of Age, Sex, KeyPop columns from data pack
# with columns named as in site densities age_option_name, sex_option_name, kp_option_name
  datapack_data  <- datapack_data %>% mutate(age_option_name = Age, 
                                             sex_option_name = Sex,
                                             kp_option_name = KeyPop) 
# modify some age_option_names in input data to match names in site densities
# do this in the newly created columns leaving originals as is
  datapack_data$age_option_name[datapack_data$age_option_name == "<01"] <- "<1" 
  datapack_data$age_option_name[datapack_data$age_option_name == "01-04"] <- "1-4"
  datapack_data$age_option_name[datapack_data$age_option_name == "05-09"] <- "5-9"
  datapack_data$age_option_name[datapack_data$age_option_name == "<= 02 months"] <- "<= 2 months"
  datapack_data$age_option_name[datapack_data$age_option_name == "02 - 12 months"] <- "2 - 12 months"


  
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
  columns <- c(names_in, "type", "percent", "siteValue", "siteValueH", "psnuValueH")
  site_tool_data <- site_tool_data %>% dplyr::rename("type" = "Support Type") %>% 
    dplyr::select(columns)
  d[["data"]][["site"]][["distributed"]] <- site_tool_data

  return(d)
  }

# wrapper function to functionalize and parallelize calls to CalculateSiteDensity
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
  
  
CalculateSiteDensity <- function(data_element_map_item, country_details, 
                                 mechanisms, dim_item_sets, base_url){
  assertthat::assert_that(NROW(data_element_map_item) == 1,
                          NROW(country_details) == 1)
  
  planning_level = as.integer(country_details$planning_level)
  
# create subsets of dim_item_sets for the relevant dimension items to be used later
  
  # dimension_set_columns <- c("age_set", "sex_set", "kp_set", "other_disagg")
  # dim_item_subsets <- purrr::map(dimension_set_columns, 
  #                                ~dplyr::filter(datapackcommons::dim_item_sets,
  #                                               model_sets ==  datapackcommons::Map19Tto20T[[3,.]]
  #                                               )
  #                                ) %>% rlang::set_names(dimension_set_columns)
  
  dim_item_sets_age <- dim_item_sets %>% 
    filter(model_sets == data_element_map_item[[1,"age_set"]])
  dim_item_sets_sex <- dim_item_sets %>% 
    filter(model_sets == data_element_map_item[[1,"sex_set"]])
  dim_item_sets_kp <-  dim_item_sets %>% 
    filter(model_sets == data_element_map_item[[1, "kp_set"]])
  dim_item_sets_other_disagg <-  dim_item_sets %>% 
    filter(model_sets == data_element_map_item[[1, "other_disagg"]])
  
# get list of dimensions (parameters) for analytics call by level {planning, community, facility}
# analytics will be called seperatly for each level  
  dimensions_by_ou_level <- BuildDimensionLists(data_element_map_item, dim_item_sets, 
                                                mechanisms, country_details)

# Grab historical data by level ierating over the list just created
  analytics_output_list <-  purrr::map(dimensions_by_ou_level, 
                                       datapackcommons::GetData_Analytics, 
                                       base_url)
  
  if(NROW(analytics_output_list$planning$results) == 0){
    return(na.omit(tibble::tribble(~indicatorCode,
                           NA_character_))) # to do return something more useful?
  }  

  # Do I need to be concerned about rounding error
  
  check_sum_1p = (sum(analytics_output_list$planning$results$Value))
  check_sum_1s = (sum(analytics_output_list$facility$results$Value) +
                    sum(analytics_output_list$community$results$Value))
  
  assertthat::assert_that(check_sum_1p == check_sum_1s)
    
  planning_data <- analytics_output_list$planning$results %>% 
    list(dim_item_sets_age, 
         dim_item_sets_sex, 
         dim_item_sets_kp,
         dim_item_sets_other_disagg) %>% 
    purrr::reduce(MapDimToOptions, allocate = "distribute") %>% 
    dplyr::mutate(psnuid = purrr::map_chr(.$ou_hierarchy, planning_level)) %>% 
    dplyr::select(-`Organisation unit`, -ou_hierarchy) %>% 
    AggByAgeSexKpOuMechSt() %>% 
    .[["processed"]] %>% 
    dplyr::select(-dplyr::one_of("count", "maximum", "minimum")) %>% 
    dplyr::rename("psnuValueH" = "Value")
  
  check_sum_2p = planning_data$psnuValueH %>% sum()
  assertthat::assert_that(check_sum_1p == check_sum_2p)
  
  ### MEchanism to Mechanism mapping would happen right here.
  ### The config fle would look something like this
  
  ### ~technical area, , ~psnu, ~old_mechanism_uid or code, ~new_mechanism_uid or code, ~weight  
  
  site_data <- dplyr::bind_rows(analytics_output_list$community$results,
                                analytics_output_list$facility$results) %>% 
    list(dim_item_sets_age, 
         dim_item_sets_sex, 
         dim_item_sets_kp,
         dim_item_sets_other_disagg) %>% 
    purrr::reduce(MapDimToOptions, allocate = "distribute")  %>% 
    dplyr::mutate(psnuid = purrr::map_chr(.$ou_hierarchy, planning_level)) %>%      
    dplyr::select(-ou_hierarchy)%>% 
    AggByAgeSexKpOuMechSt()%>% 
    .[["processed"]] %>% dplyr::select(-dplyr::one_of("count", "maximum", "minimum")) %>% 
    dplyr::rename("siteValueH" = "Value")
  
  check_sum_2s = site_data$siteValueH %>% sum()
  assertthat::assert_that(check_sum_1p == check_sum_2s)
  
  ### MEchanism to Mechanism mapping would happen right here.
  ### The config fle would look something like this
  
  ### ~technical area, , ~psnu, ~old_mechanism_uid or code, ~new_mechanism_uid or code, ~weight
  ### join data after mech to mech mapping if implementes
  joined_data <- dplyr::left_join(site_data, planning_data) %>%
    dplyr::left_join(mechanisms, by = c("Funding Mechanism" = "categoryOptionId")) %>% 
    dplyr::rename("mechanismCode" = "code")  %>% 
    select(-name, -categoryOptionComboId) %>% 
    mutate(percent = siteValueH/psnuValueH, 
           indicatorCode = data_element_map_item$indicatorCode_fy20_cop)

  check_sum_3p  <- joined_data %>% select(-dplyr::one_of("percent", "siteValueH", 
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

AggByAgeSexKpOuMechSt <- function(data) {
  #to do add assertions must include value and org unit columns
  aggregated_data <- data %>%
    dplyr::select_if(names(.) %in% c("sex_option_uid", "sex_option_name",
                              "age_option_uid", "age_option_name",
                              "kp_option_uid", "kp_option_name",
                              "Organisation unit", 
                              "Funding Mechanism", "Support Type",
                              "psnuid", "Value")) %>%
    dplyr::group_by_if(names(.) %in% c("sex_option_uid", "sex_option_name",
                                "age_option_uid", "age_option_name",
                                "kp_option_uid", "kp_option_name",
                                "Organisation unit", 
                                "Funding Mechanism", "Support Type",
                                "psnuid")) %>%
    dplyr::summarise(count = n(), minimum = min(Value), 
              maximum = max(Value), Value = sum(Value)) %>% 
    ungroup()
  
  if (max(aggregated_data$count) == 1) { # nothing aggregated
    return(list(processed = data, was_aggregated = FALSE))
  } 
  else {
    return(list(processed = aggregated_data, was_aggregated = TRUE, 
                aggregations = filter(aggregated_data, count>1)))
  }
}

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
    na.omit() # there are some items in dim item sets with no source dimension
  
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
  site_tool_data_site <- filter(site_tool_data, !is.na(siteValue))
  site_tool_data_psnu_only <- filter(site_tool_data, is.na(siteValue))
# sum of the value column in the input data by indicator code
  sum_dp = datapack_data %>% dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(original = sum(value, na.rm=TRUE)) %>% dplyr::ungroup()

# sum of data left at the psnu level (nothing to distribute to) determined by siteValue = NA
  sum_site_psnu_only  <-  
    site_tool_data_psnu_only %>% 
    dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(psnu = sum(value, na.rm=TRUE)) %>% 
    ungroup()
  
  totals <-  
    site_tool_data %>% 
    dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(site = sum(siteValue, na.rm=TRUE)) %>% 
    ungroup() %>% dplyr::full_join(sum_site_psnu_only) %>% 
    dplyr::full_join(sum_dp) %>% dplyr::group_by(indicatorCode) %>% 
    dplyr::summarise(input_total = sum(input = sum(original, na.rm=TRUE)), 
                     site_tool_total = sum(site, na.rm=TRUE) + sum(psnu, na.rm=TRUE),
                     difference = input_total - site_tool_total) %>% 
    ungroup()
  
  mechanisms_not_distributed <- dplyr::setdiff(datapack_data$mechanismCode, 
                                             site_tool_data_site$mechanismCode)
  psnus_not_distributed <- dplyr::setdiff(datapack_data$psnuid, 
                                               site_tool_data_site$psnuid)
  
  targets_not_distributed <- dplyr::setdiff(datapack_data$indicatorCode, 
                                          site_tool_data_site$indicatorCode)    
  
  indicators_ages_dp <- datapack_data %>% select(indicatorCode, Age) %>% unique()
  indicators_ages_site <- site_tool_data_site %>% select(indicatorCode, Age) %>% unique()
  indicator_age_combos_not_distributed = dplyr::anti_join(indicators_ages_dp, indicators_ages_site)
  
  indicators_sexes_dp <- datapack_data %>% select(indicatorCode, Sex) %>% unique()
  indicators_sexes_site <- site_tool_data_site %>% select(indicatorCode, Sex) %>% unique()
  indicator_sexes_combos_not_distributed = dplyr::anti_join(indicators_sexes_dp, indicators_sexes_site)
  
  indicators_kps_dp <- datapack_data %>% select(indicatorCode, KeyPop) %>% unique()
  indicators_kps_site <- site_tool_data_site %>% select(indicatorCode, KeyPop) %>% unique()
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
  
  # all_equal(d_new$data$site$distributed, d_old$data$site$distributed) 
}
