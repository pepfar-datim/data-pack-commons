# install.packages("tidyverse")
# library(tidyverse)
# library(httr)
#
# validDPDisaggs <- function() {
#
#   validDisaggs <- list(
#     "Epi Cascade I" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "Epi Cascade II" = list(
#       validAges = c("<15","15+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "Epi PMTCT" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "Prioritization" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "PMTCT_STAT_ART" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female"),
#       validKPs = NA_character_),
#     "PMTCT_EID" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_),
#     "TB_STAT_ART" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "VMMC" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Male"),
#       validKPs = NA_character_),
#     "TX" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "CXCA" = list(
#       validAges = c("25-29","30-34","35-39","40-44","45-49"),
#       validSexes = c("Female"),
#       validKPs = NA_character_),
#     "HTS" = list(
#       validAges = c("01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "TB_TX_PREV" = list(
#       validAges = c("<15","15+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "OVC" = list(
#       validAges = c("<01","01-04","05-09","10-14","15-17","18+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "KP" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = c("Female PWID","Male PWID","PWID","FSW","MSM not SW","MSM SW","MSM","People in prisons and other enclosed settings","TG SW","TG not SW","TG")),
#     "PP" = list(
#       validAges = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "PrEP" = list(
#       validAges = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"),
#       validSexes = c("Female","Male"),
#       validKPs = NA_character_),
#     "GEND" = list(
#       validAges = NA_character_,
#       validSexes = NA_character_,
#       validKPs = NA_character_)
#   )
#
#   return(validDisaggs)
# }
#
#
#
# disaggs <- validDPDisaggs() %>%
#   magrittr::extract2("TX") %>%
#   purrr::cross_df()
#
# cross_output <- tidyr::crossing(1:18189, disaggs)
#
# write_csv(cross_output, "/Users/siddharth/Desktop/Output.csv")

#Produce Site List and Mech List for use in Site Level Review Tool

#CHANGE ME!
#config_path="~/.secrets/datim.json"
config_path="/Users/sam/.secrets/prod.json"
datapackcommons::DHISLogin("/Users/sam/.secrets/prod.json")
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

OrgUnitsByLevels <- function(assignments, data) {
  country_level_in = unique(assignments$country_level)
  planning_level_in = unique(assignments$planning_level)
  community_level_in = unique(assignments$community_level)
  facility_level_in = unique(assignments$facility_level)
  assertthat::assert_that(length(country_level_in) == 1,
                          length(planning_level_in) == 1,
                          length(community_level_in) == 1,
                          length(facility_level_in) == 1)
  
  data <- data %>%  dplyr::filter(!stringr::str_detect(name, "_Military")) %>%
    dplyr::filter(level == planning_level_in |
                    level == community_level_in |
                    level == facility_level_in)
  
  country_name_col_symbol <- as.name(paste0("level", country_level_in, "name"))
  planning_name_col_symbol <- as.name(paste0("level", planning_level_in, "name"))
  planning_uid_col_symbol <- as.name(paste0("uidlevel", planning_level_in))
  data <-  dplyr::mutate(data, country_name = !!country_name_col_symbol,
                         psnu_name = !!planning_name_col_symbol,
                         psnu_uid = !!planning_uid_col_symbol)
  # if (country_level_in == 3) {
  #   data <-  dplyr::mutate(data, country_name = level3name)
  # } else {
  #   data <- dplyr::mutate(data, country_name = level4name)
  # }
  
  data <-
    dplyr::inner_join(assignments, data, 
                      by = c("country_name" = "country_name")) %>%
    dplyr::mutate(Site_Type = level)
  
  data$Site_Type[data$Site_Type == planning_level_in] <- "planning"
  data$Site_Type[data$Site_Type == facility_level_in] <- "facility"
  data$Site_Type[data$Site_Type == community_level_in] <- "community"
  # data$psnu_name[data$planning_level == 4] <- data$level4name
  # data$psnu_name[data$planning_level == 5] <- data$level5name
  # data$psnu_name[data$planning_level == 6] <- data$level6name
  # data$psnu_uid[data$planning_level == 4] <- data$uidlevel4
  # data$psnu_uid[data$planning_level == 5] <- data$uidlevel5
  # data$psnu_uid[data$planning_level == 6] <- data$uidlevel6
  
  # # Military data rows
  # military_data <- military_data %>%  dplyr::filter(stringr::str_detect(name, "_Military"))
  # # military_data <- subset(military_data, select=-(1:4))
  # military_data$Site_Type <- "Military"
  # military_data$psnu_name <- military_data$level4name
  # military_data$psnu_uid <- military_data$uidlevel4
  # if (country_level_in == 3) {
  #   military_data <-  dplyr::mutate(military_data, country_name = level3name)
  # } else {
  #   military_data <- dplyr::mutate(military_data, country_name = level4name)
  # }
  
  # data = rbind(data, military_data)
  
  return(data)
}



# Never use www.triage

# country, psnu, site, site_type

PSNU_levels <- GetCountryLevels(base_url) %>% dplyr::filter(planning_level > 0) 
orgHierarchy2 <-
  paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
  httr::GET() %>%
  httr::content(., "text") %>%
  jsonlite::fromJSON(., flatten = TRUE)

orgHierarchy2 <- as.data.frame(orgHierarchy2$rows,stringsAsFactors = FALSE) %>%
  setNames(.,orgHierarchy2$headers$name)

temp = plyr::ddply(PSNU_levels, .(country_level, planning_level, 
                                  community_level, facility_level),
                   OrgUnitsByLevels, orgHierarchy2)

temp_2 = orgHierarchy2 %>% dplyr::filter(stringr::str_detect(name,"_Military")) %>% 
  dplyr::bind_rows(temp)
get_full_site_list <- function(config_path) {
  
  # psnu_levels <-
  #   paste0(getOption("baseurl"),
  #          "api/dataStore/dataSetAssignments/ous") %>%
  #   httr::GET() %>%
  #   httr::content(., "text") %>%
  #   jsonlite::fromJSON(., flatten = TRUE) %>%
  #   do.call(rbind.data.frame, .) %>%
  #   dplyr::select(name3, prioritization,community,facility, planning) %>%
  #   dplyr::mutate(country_name = as.character(name3))
  
  PSNU_levels <- GetCountryLevels(base_url)
  
  orgHierarchy <-
    paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
    httr::GET() %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(., flatten = TRUE)
  
  ous_list_regional<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
    setNames(.,orgHierarchy$headers$name) %>%
    filter(level3name == 'Caribbean Region'| level3name == 'Central America Region' | level3name == 'Asia Regional Program' | level3name=='Central Asia Region')%>%
    dplyr::inner_join(PSNU_levels,by=c("level4name" = "country_name")) %>%  # This eliminates all the military sites for level3name = region areas
    mutate(level = as.numeric(level)) %>%
    mutate(
      psnu_name = case_when(
        planning_level == 4 ~ level4name,
        planning_level == 5 ~ level5name,
        planning_level == 6 ~ level6name),
      psnu_uid = case_when(
        planning_level == 4 ~ uidlevel4,
        planning_level == 5 ~ uidlevel5,
        planning_level == 6 ~ uidlevel6)
    ) %>%
    mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
    select(organisationunituid,name,level,planning_level,community_level,facility_level,uidlevel4,level4name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
    mutate(
      siteType = case_when(
        stringr::str_detect(name, "_Military ") ~ "Military",  # This is redundant as no rows containt military due to filtering
        level == facility_level ~ "Facility",
        level == community_level  ~ "Community",
        level == planning_level ~ "PSNU"),
      distributed=1) %>%
    select(distributed,siteType,everything())
  
  ous_list_regional_mil <- as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
    setNames(.,orgHierarchy$headers$name) %>%
    filter(level3name == 'Caribbean Region'| level3name == 'Central America Region' | level3name == 'Asia Regional Program' | level3name=='Central Asia Region')%>%
    filter(stringr::str_detect(level4name, "_Military")) %>%
    mutate(psnu_name = level4name) %>%
    mutate(psnu_uid = uidlevel4) %>%
    mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
    select(organisationunituid,name,level,uidlevel4,level4name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
    mutate(siteType = "Military")
  
  
  # Removes all rows with NA values for Site Type
  ous_list_regional <- ous_list_regional %>% 
    dplyr::filter(! is.na(siteType))
  
  ous_list_non_regional<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
    setNames(.,orgHierarchy$headers$name) %>%
    filter(level3name != 'Caribbean Region'| level3name != 'Central America Region' | level3name != 'Asia Regional Program' | level3name !='Central Asia Region')%>%
    dplyr::inner_join(PSNU_levels,by=c("level3name" = "country_name")) %>%
    mutate(level = as.numeric(level)) %>%
    mutate(
      psnu_name = case_when(
        planning_level == 4 ~ level4name,
        planning_level == 5 ~ level5name,
        planning_level == 6 ~ level6name),
      psnu_uid = case_when(
        planning_level == 4 ~ uidlevel4,
        planning_level == 5 ~ uidlevel5,
        planning_level == 6 ~ uidlevel6)
    ) %>%
    mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
    select(organisationunituid,name,level,planning_level,community_level,facility_level,uidlevel3,level3name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
    mutate(
      siteType = case_when(
        stringr::str_detect(name, "_Military ") ~ "Military",
        level == facility_level ~ "Facility",
        level == community_level  ~ "Community",
        level == planning_level ~ "PSNU"),
      distributed=1) %>%
    select(distributed,siteType,everything())
  
  # Removes all rows with NA values for Site Type
  ous_list_non_regional <- ous_list_non_regional %>% 
    dplyr::filter(! is.na(siteType))
  
  #Add in _Military sites again for cases where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(siteType=="Military") %>%
    mutate(distributed=0) %>%
    bind_rows(ous_list,.)
  
  #Add in _Military sites again for cases where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(siteType=="Military") %>%
    mutate(distributed=0) %>%
    bind_rows(ous_list,.)
  
  #Add in non-clustered PSNUs where these cannot be distributed, as indicated in distribution function
  # ous_list <- ous_list %>%
  #   filter(level==prioritization & !str_detect(name,"_Military")) %>%
  #   mutate(siteType="PSNU",
  #          distributed=0) %>%
  #   bind_rows(ous_list,.) %>%
  #   #Filter out anything not tagged at this point
  #   filter(!is.na(siteType)) %>%
  #   #Construct Site Tool Name
  #   mutate(DataPackSiteID=case_when(distributed==0 ~ paste0(name," > NOT YET DISTRIBUTED (",organisationunituid,")"),
  #                                   siteType=="Military" ~ paste0(name," (",organisationunituid,")"),
  #                                   siteType=="Facility" ~ paste0(dpOrgUnit," > ",name," {Facility} (",organisationunituid,")"),
  #                                   siteType=="Community" ~ paste0(dpOrgUnit," > ",name," {Community} (",organisationunituid,")"))) %>%
  #   select(DataPackSiteUID=organisationunituid,DataPackSiteID,ou_uid=uidlevel4,ou_name=level4name,siteType,distributed) %>%
  #   unique()
  
  # #Creating dataframe for country at level 3 sites
  # ous_list_2<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
  #   setNames(.,orgHierarchy$headers$name) %>%
  #   filter(level3name != 'Caribbean Region'| level3name != 'Central America Region' | level3name != 'Asia Regional Program' | level3name !='Central Asia Region')%>%
  #   dplyr::inner_join(psnu_levels,by=c("level3name" = "name3")) %>%
  #   mutate(level = as.numeric(level)) %>%
  #   mutate(
  #     psnu_name = case_when(
  #       prioritization == 4 ~ level4name,
  #       prioritization == 5 ~ level5name,
  #       prioritization == 6 ~ level6name),
  #     psnu_uid = case_when(
  #       prioritization == 4 ~ uidlevel4,
  #       prioritization == 5 ~ uidlevel5,
  #       prioritization == 6 ~ uidlevel6)
  #   ) %>%
  #   mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
  #   select(organisationunituid,name,level,prioritization,community,facility,uidlevel3,level3name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
  #   mutate(
  #     siteType = case_when(
  #       stringr::str_detect(name, "_Military ") ~ "Military",
  #       level == facility ~ "Facility",
  #       level == community  ~ "Community"),
  #     distributed=1) %>%
  #   select(distributed,siteType,everything())
  # 
  # #Add in _Military sites again for cases where these cannot be distributed, as indicated in distribution function
  # ous_list_2 <- ous_list_2 %>%
  #   filter(siteType=="Military") %>%
  #   mutate(distributed=0) %>%
  #   bind_rows(ous_list_2,.)
  # 
  # #Add in non-clustered PSNUs where these cannot be distributed, as indicated in distribution function
  # ous_list_2 <- ous_list_2 %>%
  #   filter(level==prioritization & !str_detect(name,"_Military")) %>%
  #   mutate(siteType="PSNU",
  #          distributed=0) %>%
  #   bind_rows(ous_list_2,.) %>%
  #   #Filter out anything not tagged at this point
  #   filter(!is.na(siteType)) %>%
  #   #Construct Site Tool Name
  #   mutate(DataPackSiteID=case_when(distributed==0 ~ paste0(name," > NOT YET DISTRIBUTED (",organisationunituid,")"),
  #                                   siteType=="Military" ~ paste0(name," (",organisationunituid,")"),
  #                                   siteType=="Facility" ~ paste0(dpOrgUnit," > ",name," {Facility} (",organisationunituid,")"),
  #                                   siteType=="Community" ~ paste0(dpOrgUnit," > ",name," {Community} (",organisationunituid,")"))) %>%
  #   select(DataPackSiteUID=organisationunituid,DataPackSiteID,ou_uid=uidlevel3,ou_name=level3name,siteType,distributed) %>%
  #   unique()
  
  # total_ous_list <- rbind(ous_list, ous_list_2)
  # 
  # return(total_ous_list)
  
  return(ous_list)
}

getMechList <- function(config_path) {
  
  url<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv")
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("mechanism","code","uid","ou")])
  
}

full_site_list <- get_full_site_list(config_path)

full_mech_list <- getMechList(config_path)

# In case the site list and mech list need to be stored as RDA files on local
# site_list <- get_site_list(config_path) %>%
#   saveRDS(paste0(outFolder,"/ous_list.rda"))

# getMechList(config_path) %>%
#   saveRDS(paste0(outFolder,"/mech_list.rda"))

# full_site_list <- readRDS("/Users/siddharth/Desktop/ous_list.rda")

#full_mech_list <- readRDS("/Users/siddharth/Desktop/mech_list.rda")

colnames(full_site_list)[4] <- "country_name"
print(full_site_list[4])


merged_table <- merge(full_site_list, PSNU_levels, by = "country_name")

temp=dplyr::anti_join(full_site_list, PSNU_levels)

# There's a problem here, the merged table has 107k rows and the full site list has 111k rows.

subset_table <- merged_table[c("country_name", "DataPackSiteID", "DataPackSiteUID", "siteType")]
