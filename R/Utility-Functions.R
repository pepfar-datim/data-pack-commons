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

#' @title CopSitesNonMilitary(assignments, flat_ouhierarchy)
#' 
#' @description Takes the assignments table containing all the countries and their site levels for different types
#' as well as the orghierarchy data containing all the sites, to combine and give site type and psnu name, id for each site 
#' Only works for countries with the same country_level, planning_level, community_level, facility_level.
#' Designed to work with ddply over an unfilter assignments table e.g. 
#'  plyr::ddply(assignments, plyr::.(country_level,planning_level,community_level,facility_level),
#'  CopSitesNonMilitary,flat_ouhierarchy)
#'  Called by AllSitesList
#' @param assignments dataframe - contains data of each country, their planning, community, facility and country level: api/dataStore/dataSetAssignments/ous
#' @param flat_ouhierarchy dataframe - List of all sites and their data with level names and UIDs, as well as site levels: /api/sqlViews/kEtZ2bSQCu2/data.json
#' @return  dataframe containing all sites - non military and their site types, their psnu names, psnu uids and country/id
#'
CopSitesNonMilitary <- function(assignments, flat_ouhierarchy) {
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

# reduce flat ou)hierarchy to non_mil and at one of the relevent hierarchy levels  
  flat_ouhierarchy <-
    flat_ouhierarchy %>%  dplyr::filter(!stringr::str_detect(name, "_Military")) %>%
    dplyr::filter(level == planning_level_in |
                    level == community_level_in |
                    level == facility_level_in)

# store the column name in flat_ouhierarchy corresponding to the different levels
  # country, planning
  # stored as variable name with as.name to be used in mutate below
  country_name_col_symbol <-
    as.name(paste0("level", country_level_in, "name"))
  planning_name_col_symbol <-
    as.name(paste0("level", planning_level_in, "name"))
  planning_uid_col_symbol <-
    as.name(paste0("uidlevel", planning_level_in))
  flat_ouhierarchy <-
    dplyr::mutate(
      flat_ouhierarchy,
      country_name = !!country_name_col_symbol,
      psnu_name = !!planning_name_col_symbol,
      psnu_uid = !!planning_uid_col_symbol
    )
  ## do we actually need to do this join???
  ## we can just filter by flat_ouhierarchy#country_name in assignments country_name
  flat_ouhierarchy <-
    dplyr::inner_join(assignments, flat_ouhierarchy,
                      by = c("country_name" = "country_name")) %>%
    dplyr::mutate(Site_Type = level)
  
  flat_ouhierarchy$Site_Type[flat_ouhierarchy$Site_Type == planning_level_in] <- "psnu"
  flat_ouhierarchy$Site_Type[flat_ouhierarchy$Site_Type == facility_level_in] <- "facility"
  flat_ouhierarchy$Site_Type[flat_ouhierarchy$Site_Type == community_level_in] <-
    "community"
  
  return(flat_ouhierarchy)
}

#' @title MilitarySiteData(flat_ouhierarchy)
#' 
#' @description Takes all site data and filters out the military sites, generating their PSNU name and ID as well
#' @param flat_ouhierarchy - flat_ouhierarchy dataframe - List of all sites and their data 
#' with level names and UIDs, as well as site levels: /api/sqlViews/kEtZ2bSQCu2/data.json
#' @return  dataframe with all military sites and their PSNU names, IDs
#'
MilitarySiteData <- function(flat_ouhierarchy) {
  military_data <- flat_ouhierarchy %>% dplyr::filter(stringr::str_detect(name, "_Military"))
  
  # assumption is all military psnus are at level 4, check this is still true
  dplyr::filter(military_data, level != 4) %>% NROW() %>% 
  {assertthat::assert_that(assertthat::are_equal(., 0), 
                             msg = "Org unit with name like _Military exists at hierarchy level other than 4")}
  
  military_data <- military_data %>%
    dplyr::mutate(Site_Type = "Military") %>%
    dplyr::mutate(psnu_name = level4name) %>% # All military at level 4
    dplyr::mutate(psnu_uid = uidlevel4) %>%
    dplyr::mutate(country_name = level3name) %>% # might be a region
    dplyr::mutate(id = uidlevel3)
  return(military_data)
}

#' @export
#' @title AllSitesList(assignments, flat_ouhierarchy)
#' 
#' @description Takes the assignments table containing all the countries and their site levels for different types
#' as well as the orghierarchy data containing all the sites, to combine and give site type and psnu name, id for each site 
#' @param assignments dataframe - contains data of each country, their planning, community, 
#' facility and country level: api/dataStore/dataSetAssignments/ous
#' @param flat_ouhierarchy dataframe - List of all sites and their data with level names and UIDs, 
#' as well as site levels: /api/sqlViews/kEtZ2bSQCu2/data.json
#' @return  dataframe containing all sites and their site types, their psnu names, psnu uids and country/id
#'
AllSitesList <- function(assignments, flat_ouhierarchy) {
  # This df contains all the non military sites
  non_mil_output = plyr::ddply(
    assignments,
    plyr::.(
      country_level,
      planning_level,
      community_level,
      facility_level
    ),
    CopSitesNonMilitary,
    flat_ouhierarchy
  )
  
  # This df contains all the military sites
  mil_output <- MilitarySiteData(flat_ouhierarchy)
  
  # Check to see if you obtain anything from military site data or non mil data before binding
  dplyr::bind_rows(non_mil_output, mil_output)
}
