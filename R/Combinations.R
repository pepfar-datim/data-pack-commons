#Produce Site List and Mech List for use in Site Level Review Tool

#CHANGE ME!
#config_path="~/.secrets/datim.json"
config_path = "/Users/siddharth/.secrets/datim.json"
datapackcommons::DHISLogin("/Users/siddharth/.secrets/datim.json")
base_url <- getOption("baseurl")
#outFolder="/home/jason/consultancy/datim/datapack/"
#outFolder="/Users/siddharth/Desktop/"

#DON'T CHANGE!
require(devtools)
#install_github("pepfar-datim/data-pack-commons", ref = "master")
require(datapackcommons)
require(plyr)

require(dplyr)
require(httr)
require(jsonlite)
require(stringr)

NonMilitarySiteData <- function(assignments, data) {
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
  data$Site_Type[data$Site_Type == community_level_in] <- "community"
  
  return(data)
}


# Never use www.triage

# country, psnu, site, site_type

PSNU_levels <-
  datapackcommons::GetCountryLevels(base_url) %>% dplyr::filter(planning_level > 0)
orgHierarchy2 <-
  paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
  httr::GET() %>%
  httr::content(., "text") %>%
  jsonlite::fromJSON(., flatten = TRUE)

orgHierarchy2 <-
  as.data.frame(orgHierarchy2$rows, stringsAsFactors = FALSE) %>%
  setNames(., orgHierarchy2$headers$name)

# This df contains all the military sites
MilitarySiteData <- function(org_data) {
  military_data = org_data %>% dplyr::filter(stringr::str_detect(name, "_Military")) %>%
    dplyr::mutate(Site_Type = "Military") %>%
    dplyr::mutate(psnu_name = level4name) %>%
    dplyr::mutate(psnu_uid = uidlevel4) %>%
    dplyr::mutate(country_name = level3name) %>%
    dplyr::mutate(id = uidlevel3)
  return(military_data)
}

temp_2 <- MilitarySiteData(orgHierarchy2)

#' @export
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

all_sites <- all_sites_list(PSNU_levels, orgHierarchy2)

getMechList <- function(config_path) {
  url <-
    paste0(getOption("baseurl"), "api/sqlViews/fgUtV6e9YIX/data.csv")
  d <- read.csv(url, stringsAsFactors = FALSE)
  return(d[, c("mechanism", "code", "uid", "ou")])
  
}
