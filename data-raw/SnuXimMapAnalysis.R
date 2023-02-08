# This script uses a custom SQL view not in prod.
# It may need to be imported into cop-test.
# Metadata import file is here:
# data-pack-commons/data-raw/get _dataelementgroupsetstructure resource table.xml

library(datapackr)
library(magrittr)
library(tidyverse)

getStandardDataElementGroups <- function() {
  datimutils::getSqlView(sql_view_uid = "m7qxbPHsikm") %>%
    dplyr::select(data_element = dataelementname,
                  data_element_uid = dataelementuid,
                  data_element_code = dataelementcode,
                  technical_area  = `Technical Area`,
                  technical_area_uid = LxhLO68FcXm,
                  targets_results = `Targets / Results`,
                  targets_results_uid = IeMmjHyBUpi,
                  support_type = `Support Type`,
                  support_type_uid = `TWXpUVE2MqL`,
                  numerator_denominator = `Numerator / Denominator`,
                  numerator_denominator_uid = `lD2x0c8kywj`,
                  disaggregation_type = `Disaggregation Type`,
                  disaggregation_type_uid = HWPJnUTMjEq
                  )
}

getDataSets_Detailed <- function(dataset_uids) {
  purrr::map(dataset_uids,
             ~ datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                      variable_keys = c("dataSets"),
                                      variable_values = c(.x))) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-dataelementdesc, -shortname) %>%
    dplyr::rename(data_element = "dataelement",
                  data_element_code = "code",
                  data_element_uid = "dataelementuid",
                  category_option_combo = "categoryoptioncombo",
                  category_option_combo_code = "categoryoptioncombocode",
                  category_option_combo_uid = "categoryoptioncombouid") %>%
    dplyr::left_join(getStandardDataElementGroups())
}

#secrets <- "/Users/sam/.secrets/cop.json"
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "coptest.json"))

#TODO ask sam about the code below as it currently does not run, GetSqlView does not find a valid sql view
fy_22_t <- datapackr::getDatasetUids(2022 - 1, "mer_targets") %>%
  getDataSets_Detailed() %>%
  dplyr::select(-dataset) %>%
  distinct()

fy_23_t <- datapackr::getDatasetUids(2023 - 1, "mer_targets") %>%
  getDataSets_Detailed() %>%
  dplyr::select(-dataset) %>%
  distinct()

fy_24_t <- c("dA9C5bL44NX", #MER Target Setting: PSNU (Facility and Community Combined) (TARGETS)
             "A2GxohPT9Hw", #MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY (TARGETS)
             "vpDd67HlZcT") %>% #
  getDataSets_Detailed() %>%
  dplyr::select(-dataset) %>%
  distinct()


dif_22T_24T <- dplyr::setdiff(fy_24_t, fy_22_t)
dif_22T_23T <- dplyr::setdiff(fy_23_t, fy_22_t)
dif_23T_22T <- dplyr::setdiff(fy_22_t, fy_23_t)
dif_23T_24T <- dplyr::setdiff(fy_24_t, fy_23_t)
dif_24T_23T <- dplyr::setdiff(fy_23_t, fy_24_t)

match_22T_23T <- dplyr::intersect(fy_23_t, fy_22_t)
match_23T_24T <- dplyr::intersect(fy_24_t, fy_23_t)


schema <- datapackr::cop22_data_pack_schema %>%
  dplyr::filter(col_type == "target"
                & dataset == "mer"
                & sheet_name != "PSNUxIM")

dplyr::setdiff(c(schema$dataelement_dsd, schema$dataelement_ta), fy_22_t$data_element_uid)
dplyr::setdiff(fy_22_t$data_element_uid, c(schema$dataelement_dsd, schema$dataelement_ta))

map <- datapackcommons::Map22Tto23T

temp <- dplyr::full_join(fy_23_t, map, by = c(technical_area_uid = "technical_area_uid",
                                              disaggregation_type_uid = "disagg_type_uid",
                                              numerator_denominator_uid = "num_or_den_uid"))
names(map)



data <- readr::read_rds("/Users/sam/COP data/PSNUxIM_20220110_1.rds") %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::group_by(indicator_code,
                  age_option_name,
                  sex_option_name,
                  kp_option_name) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(valid_ages.name = "age_option_name",
                valid_sexes.name = "sex_option_name",
                valid_kps.name = "kp_option_name") %>%
  dplyr::full_join(datapackr::cop22_map_DataPack_DATIM_DEs_COCs) %>%
  dplyr::filter(!stringr::str_detect(dataelementname, "SUBNAT"))

##########
##standard_de_groups <- datapackcommons::GetSqlView("vqetpxYlX1c") ## jason.datim
 ## test mer 2




# sql view

# select
# dataelement data_element,
# dataelementshortname data_element_short_name,
# dataelementuid data_element_uid,
# dataelementcode data_element_code,
# technicalarea technical_area,
# technicalareauid technical_area_uid,
# numordenom num_or_denom,
# numordenomuid num_or_denom_uid,
# supporttype support_type,
# supporttypeuid support_type_uid,
# disaggregationtype disagregation_type,
# disaggregationtypeuid disagregation_type_uid,
# targetorresult target_or_result,
# targetorresultuid target_or_result_uid
# from
# (
#   select
#   de.name dataelement,
#   de.shortname dataelementshortname,
#   de.uid dataelementuid,
#   de.code dataelementcode,
#   ta_deg.name technicalarea,
#   ta_deg.shortname technicalareashortname,
#   ta_deg.uid technicalareauid,
#   ta_deg.code technicalareacode
#   from
#   dataelementgroupset ta
#   inner join dataelementgroupsetmembers ta_degsm on
#   ta.dataelementgroupsetid = ta_degsm.dataelementgroupsetid
#   inner join dataelementgroup ta_deg on
#   ta_deg.dataelementgroupid = ta_degsm.dataelementgroupid
#   inner join dataelementgroupmembers ta_degm on
#   ta_degm.dataelementgroupid = ta_deg.dataelementgroupid
#   right join dataelement de on
#   ta_degm.dataelementid = de.dataelementid
#   where
#   ta.uid = 'LxhLO68FcXm') ta
# left join (
#   select
#   nord_de.uid nord_dataelementuid,
#   nord_deg.name numordenom,
#   nord_deg.shortname numordenomshortname,
#   nord_deg.uid numordenomuid,
#   nord_deg.code numordenomcode
#   from
#   dataelementgroupset nord
#   inner join dataelementgroupsetmembers nord_degsm on
#   nord.dataelementgroupsetid = nord_degsm.dataelementgroupsetid
#   inner join dataelementgroup nord_deg on
#   nord_deg.dataelementgroupid = nord_degsm.dataelementgroupid
#   inner join dataelementgroupmembers nord_degm on
#   nord_degm.dataelementgroupid = nord_deg.dataelementgroupid
#   inner join dataelement nord_de on
#   nord_degm.dataelementid = nord_de.dataelementid
#   where
#   nord.uid = 'lD2x0c8kywj' ) nord on
# nord.nord_dataelementuid = ta.dataelementuid
# left join (
#   select
#   st_de.uid st_dataelementuid,
#   st_deg.name supporttype,
#   st_deg.shortname supporttypeshortname,
#   st_deg.uid supporttypeuid,
#   st_deg.code supporttypecode
#   from
#   dataelementgroupset st
#   inner join dataelementgroupsetmembers st_degsm on
#   st.dataelementgroupsetid = st_degsm.dataelementgroupsetid
#   inner join dataelementgroup st_deg on
#   st_deg.dataelementgroupid = st_degsm.dataelementgroupid
#   inner join dataelementgroupmembers st_degm on
#   st_degm.dataelementgroupid = st_deg.dataelementgroupid
#   inner join dataelement st_de on
#   st_degm.dataelementid = st_de.dataelementid
#   where
#   st.uid = 'TWXpUVE2MqL' ) st on
# st.st_dataelementuid = ta.dataelementuid
# left join (
#   select
#   dt_de.uid dt_dataelementuid,
#   dt_deg.name disaggregationtype,
#   dt_deg.shortname disaggregationtypeshortname,
#   dt_deg.uid disaggregationtypeuid,
#   dt_deg.code disaggregationtypecode
#   from
#   dataelementgroupset dt
#   inner join dataelementgroupsetmembers dt_degsm on
#   dt.dataelementgroupsetid = dt_degsm.dataelementgroupsetid
#   inner join dataelementgroup dt_deg on
#   dt_deg.dataelementgroupid = dt_degsm.dataelementgroupid
#   inner join dataelementgroupmembers dt_degm on
#   dt_degm.dataelementgroupid = dt_deg.dataelementgroupid
#   inner join dataelement dt_de on
#   dt_degm.dataelementid = dt_de.dataelementid
#   where
#   dt.uid = 'HWPJnUTMjEq' ) dt on
# dt.dt_dataelementuid = ta.dataelementuid
# left join (
#   select
#   tr_de.uid tr_dataelementuid,
#   tr_deg.name targetorresult,
#   tr_deg.shortname targetorresultshortname,
#   tr_deg.uid targetorresultuid,
#   tr_deg.code targetorresultcode
#   from
#   dataelementgroupset tr
#   inner join dataelementgroupsetmembers tr_degsm on
#   tr.dataelementgroupsetid = tr_degsm.dataelementgroupsetid
#   inner join dataelementgroup tr_deg on
#   tr_deg.dataelementgroupid = tr_degsm.dataelementgroupid
#   inner join dataelementgroupmembers tr_degm on
#   tr_degm.dataelementgroupid = tr_deg.dataelementgroupid
#   inner join dataelement tr_de on
#   tr_degm.dataelementid = tr_de.dataelementid
#   where
#   tr.uid = 'IeMmjHyBUpi' ) tr on
# tr.tr_dataelementuid = ta.dataelementuid;
