library(datapackr)
library(tidyverse)

secrets <- "/Users/sam/.secrets/testmer2.json"
datapackr::loginToDATIM(secrets)
base_url = options("baseurl")

##standard_de_groups <- datapackcommons::GetSqlView("vqetpxYlX1c") ## jason.datim
standard_de_groups <- datapackcommons::GetSqlView("wJno1xqHAeB") ## test mer 2

fy_20_t <- datapackcommons::GetSqlView("DotdxKrNZxG", 
                                       c("dataSets"), 
                                       c("sBv1dj90IX6")) %>% 
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("nIHNMxuPUOR"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("C2G7IyPPrvD"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("HiJieecLXxN"))) %>% 
  dplyr::select(fy_20_t_dataelementuid = dataelementuid, fy_20_t_name = dataelement) %>% 
  distinct() %>% left_join(standard_de_groups, by = c(fy_20_t_dataelementuid = "data_element_uid")) %>% 
  dplyr::filter(support_type == "DSD") %>% 
  dplyr::select(fy_20_t_dataelementuid, technical_area, num_or_denom, disaggregation_type)



schema <- datapackr::cop20_data_pack_schema %>% dplyr::filter(col_type == "target"
                                                        & dataset == "mer") %>% 
  dplyr::left_join(standard_de_groups, by = c("dataelement_dsd" = "data_element_uid"))


temp=dplyr::full_join(schema, fy_20_t, by = c("technical_area"="technical_area", "num_or_denom"="num_or_denom", "disaggregation_type"="disaggregation_type"))

map <- datapackcommons::Map20Tto21T
fy_20_t <- datapackcommons::GetSqlView("DotdxKrNZxG", 
                                       c("dataSets"), 
                                       c("sBv1dj90IX6")) %>% 
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("nIHNMxuPUOR"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("C2G7IyPPrvD"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("HiJieecLXxN"))) %>% 
  dplyr::select(dataelementuid, fy_20_name = dataelement) %>% 
  distinct() %>% left_join(standard_de_groups, by = c(dataelementuid = "data_element_uid"))

#any indidacotrs in map no longer relevant?

dplyr::setdiff(map$indicatorCode_fy20_cop, schema$indicator_code)


temp = dplyr::left_join(schema, standard_de_groups, by = c("dataelement_dsd" = "data_element_uid")) %>% 
  dplyr::full_join(datapackcommons::Map20Tto21T, by = c("indicator_code" = "indicatorCode_fy20_cop")) %>% 
  dplyr::full_join(fy_20_t, by = c("dataelement_dsd" = "dataelementuid"))


temp2 = dplyr::select(temp, indicator_code, 
                      valid_ages, age_set, 
                      valid_sexes, sex_set,
                      valid_kps, kp_set, 
                      technical_area.x, technical_area.y,
                      num_or_den, num_or_denom,
                      disagg_type, dissagregation_type)


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