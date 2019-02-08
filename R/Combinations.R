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
config_path="/Users/sam/.secrets/triage.json"
#outFolder="/home/jason/consultancy/datim/datapack/"
#outFolder="/Users/siddharth/Desktop/"

#DON'T CHANGE!

require(dplyr)
require(httr)
require(jsonlite)
require(stringr)

PSNU_levels <- GetCountryLevels("https://www.datim.org/")

get_full_site_list <- function(config_path) {

  LoadConfig(config_path)

  psnu_levels <-
    paste0(getOption("baseurl"),
           "api/dataStore/dataSetAssignments/ous") %>%
    GET() %>%
    content(., "text") %>%
    fromJSON(., flatten = TRUE) %>%
    do.call(rbind.data.frame, .) %>%
    select(name3, prioritization,community,facility) %>%
    mutate(name3 = as.character(name3))


  orgHierarchy <-
    paste0(getOption("baseurl"), "/api/sqlViews/kEtZ2bSQCu2/data.json") %>%
    GET() %>%
    content(., "text") %>%
    fromJSON(., flatten = TRUE)


  ous_list<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>%
    setNames(.,orgHierarchy$headers$name) %>%
    dplyr::inner_join(psnu_levels,by=c("level3name" = "name3")) %>%
    mutate(level = as.numeric(level)) %>%
    mutate(
      psnu_name = case_when(
        prioritization == 4 ~ level4name,
        prioritization == 5 ~ level5name,
        prioritization == 6 ~ level6name),
      psnu_uid = case_when(
        prioritization == 4 ~ uidlevel4,
        prioritization == 5 ~ uidlevel5,
        prioritization == 6 ~ uidlevel6)
    ) %>%
    mutate(dpOrgUnit=psnu_name, dpOrgUnitUID=psnu_uid) %>%
    select(organisationunituid,name,level,prioritization,community,facility,uidlevel3,level3name,psnu_name,psnu_uid,dpOrgUnit,dpOrgUnitUID) %>%
    mutate(
      siteType = case_when(
        stringr::str_detect(name, "_Military ") ~ "Military",
        level == facility ~ "Facility",
        level == community  ~ "Community"),
      distributed=1) %>%
    select(distributed,siteType,everything())

  #Add in _Military sites again for cases where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(siteType=="Military") %>%
    mutate(distributed=0) %>%
    bind_rows(ous_list,.)

  #Add in non-clustered PSNUs where these cannot be distributed, as indicated in distribution function
  ous_list <- ous_list %>%
    filter(level==prioritization & !str_detect(name,"_Military")) %>%
    mutate(siteType="PSNU",
           distributed=0) %>%
    bind_rows(ous_list,.) %>%
    #Filter out anything not tagged at this point
    filter(!is.na(siteType)) %>%
    #Construct Site Tool Name
    mutate(DataPackSiteID=case_when(distributed==0 ~ paste0(name," > NOT YET DISTRIBUTED (",organisationunituid,")"),
                                    siteType=="Military" ~ paste0(name," (",organisationunituid,")"),
                                    siteType=="Facility" ~ paste0(dpOrgUnit," > ",name," {Facility} (",organisationunituid,")"),
                                    siteType=="Community" ~ paste0(dpOrgUnit," > ",name," {Community} (",organisationunituid,")"))) %>%
    select(DataPackSiteUID=organisationunituid,DataPackSiteID,ou_uid=uidlevel3,ou_name=level3name,siteType,distributed) %>%
    unique()

  return(ous_list)
}

getMechList <- function(config_path) {

  LoadConfig(config_path)
  url<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv")
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("mechanism","code","uid","ou")])

}

full_site_list <- get_full_site_list(config_path)

fill_mech_list <- getMechList(config_path)

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
# There's a problem here, the merged table has 107k rows and the full site list has 111k rows.

subset_table <- merged_table[c("country_name", "DataPackSiteID", "DataPackSiteUID", "siteType")]
