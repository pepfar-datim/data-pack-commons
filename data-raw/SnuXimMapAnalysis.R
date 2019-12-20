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
