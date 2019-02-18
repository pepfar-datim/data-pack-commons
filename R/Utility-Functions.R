#' @export
#' @title StackPrefixedCols(data, prefixes)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
StackPrefixedCols <- function(data, prefixes){
  assertthat::assert_that(length(prefixes) > 0, is.data.frame(data))
  SelectAndStripPrefix <- function(prefix, df) {
    dplyr::select(df, dplyr::starts_with(prefix, ignore.case = FALSE)) %>% 
      dplyr::rename_all(.funs = stringr::str_remove, pattern = prefix)
  }
  
  purrr::map(prefixes, SelectAndStripPrefix, data) %>% dplyr::bind_rows()
}

#' @export
#' @title OrgUnitsByLevels(assignments, data)
#' 
#' @description Takes the assignments table containing all the countries and their site levels for different types
#' as well as the orghierarchy data containing all the sites, to combine and give site type and psnu name, id for each site 
#' @param assignments dataframe - contains data of each country, their planning, community, facility and country level: /api/sqlViews/kEtZ2bSQCu2/data.json
#' @param data dataframe - List of all sites and their data with level names and UIDs, as well as site levels: api/dataStore/dataSetAssignments/ous
#' @return  dataframe containing all sites and their site types, their psnu names, psnu uids and country/id
#'
CopSitesNonMilitary <- function(assignments, data) {
  country_level_in = unique(assignments$country_level)
  planning_level_in = unique(assignments$planning_level)
  community_level_in = unique(assignments$community_level)
  facility_level_in = unique(assignments$facility_level)
  assertthat::assert_that(
    length(country_level_in) == 1,
    length(planning_level_in) == 1,
    length(community_level_in) == 1,
    length(facility_level_in) == 1
  )
  
  data <-
    data %>%  dplyr::filter(!stringr::str_detect(name, "_Military")) %>%
    dplyr::filter(level == planning_level_in |
                    level == community_level_in |
                    level == facility_level_in)
  
  country_name_col_symbol <-
    as.name(paste0("level", country_level_in, "name"))
  planning_name_col_symbol <-
    as.name(paste0("level", planning_level_in, "name"))
  planning_uid_col_symbol <-
    as.name(paste0("uidlevel", planning_level_in))
  data <-
    dplyr::mutate(
      data,
      country_name = !!country_name_col_symbol,
      psnu_name = !!planning_name_col_symbol,
      psnu_uid = !!planning_uid_col_symbol
    )
  
  data <-
    dplyr::inner_join(assignments, data,
                      by = c("country_name" = "country_name")) %>%
    dplyr::mutate(Site_Type = level)
  
  data$Site_Type[data$Site_Type == planning_level_in] <- "planning"
  data$Site_Type[data$Site_Type == facility_level_in] <- "facility"
  data$Site_Type[data$Site_Type == community_level_in] <-
    "community"
  
  return(data)
}

#' @export
#' @title StackPrefixedCols(data, prefixes)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
MilitarySiteData <- function(org_data) {
  military_data = org_data %>% dplyr::filter(stringr::str_detect(name, "_Military")) %>%
    dplyr::mutate(Site_Type = "Military") %>%
    dplyr::mutate(psnu_name = level4name) %>%
    dplyr::mutate(psnu_uid = uidlevel4) %>%
    dplyr::mutate(country_name = level3name) %>%
    dplyr::mutate(id = uidlevel3)
  return(military_data)
}

#' @export
#' @title StackPrefixedCols(data, prefixes)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
all_sites_list <- function(assignments, org_data) {
  # This df contains all the non military sites
  temp = plyr::ddply(
    assignments,
    .(
      country_level,
      planning_level,
      community_level,
      facility_level
    ),
    NonMilitarySiteData,
    org_data
  )
  
  # This df contains all the military sites
  temp_2 <- MilitarySiteData(org_data)
  
  return(all_sites <- dplyr::bind_rows(temp, temp_2))
}
