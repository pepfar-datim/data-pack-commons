library(tidyverse)

datapackcommons::DHISLogin("/users/sam/.secrets/triage.json")
base_url <- getOption("baseurl")
get_indicator_details <- function(uid){
  datapackcommons::getMetadata(base_url,
                               "indicators",
                               glue::glue("id:eq:{uid}"),
                               "name, id, numerator, denominator") 
}

extract_formula_components <- function(formula){
  stringr::str_split(formula, "[+-]")[[1]] %>%
    stringr::str_remove_all(" ") %>% 
    stringr::str_remove_all("\\#") %>% 
    stringr::str_remove_all("\\{") %>% 
    stringr::str_remove_all("\\}") %>% tibble::enframe(name=NULL) %>% 
    dplyr::mutate(data_element_uid = stringr::str_sub(value, 1,11)) %>% 
    dplyr::mutate(co_combination_uid = dplyr::if_else(stringr::str_length(value) > 11 ,
                                                      stringr::str_sub(value, 13,23),
                                                      "")) %>% 
    dplyr::select(- value)
  
}



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
  distinct()

fy_19_t <- datapackcommons::GetSqlView("DotdxKrNZxG", 
                                       c("dataSets"), 
                                       c("BWBS39fydnX")) %>% 
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("l796jk9SW7q"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("X8sn5HE5inC"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("eyI0UOWJnDk"))) %>% 
  distinct()
            

fy_19_r <- datapackcommons::GetSqlView("DotdxKrNZxG", 
                                       c("dataSets"), 
                                       c("KWRj80vEfHU")) %>% 
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("fi9yMqWLWVy"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("zUoy5hk8r0q"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("PyD4x9oFwxJ"))) %>% 
  distinct()

fy_18_r <- datapackcommons::GetSqlView("DotdxKrNZxG", 
                                       c("dataSets"), 
                                       c("uN01TT331OP")) %>% 
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("WbszaIdCi92"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("BxIx51zpAjh"))) %>%
  dplyr::bind_rows(datapackcommons::GetSqlView("DotdxKrNZxG", 
                                               c("dataSets"), 
                                               c("tz1bQ3ZwUKJ"))) %>% 
  distinct()

elements_fy19r_fy20t <- dplyr::bind_rows(fy_19_r, fy_20_t) %>% dplyr::select(-dataset) %>% dplyr::distinct()
data_required <- datapackcommons::data_required


# -------------------------------------------------------------------------


get_invalid_data_elements <- function(data_required_spec){
  print(data_required_spec)
indicators  <-  c(data_required_spec$A.dx_id,
                  data_required_spec$B.dx_id) %>% 
  unique() %>% 
  na.omit() %>% 
  purrr::map(get_indicator_details) %>% 
  purrr::compact() %>% 
  dplyr::bind_rows()

if(length(indicators) == 0 ){
  return(NULL)
}

numerator_components <- indicators$numerator %>% 
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() 

numerator_data_elements <- numerator_components %>% .[["data_element_uid"]] %>% unique() %>%
  purrr::map(~datapackcommons::getMetadata(base_url, 
                                           "dataElements", 
                                           glue::glue("id:eq:{.x}"))) %>% 
  dplyr::bind_rows()


denominator_components <- indicators$denominator[indicators$denominator != 1] %>%  
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() 

denominator_data_elements <- denominator_components %>% 
  .[["data_element_uid"]] %>% 
  unique() %>%
  purrr::map(~datapackcommons::getMetadata(base_url, 
                                           "dataElements", 
                                           glue::glue("id:eq:{.x}"))) %>% 
  dplyr::bind_rows()



combined_data_elements <- dplyr::bind_rows(numerator_data_elements,denominator_data_elements)

dplyr::setdiff(combined_data_elements$displayName, elements_fy19r_fy20t$dataelement)
}

plyr::alply(data_required, 1, get_invalid_data_elements)






indicators_with_co_combos <- dplyr::filter(indicators,
                                           stringr::str_detect(numerator,"\\.") |
                                             stringr::str_detect(denominator,"\\."))






numerator_components <- indicators$numerator %>% 
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() %>% .[["data_element_uid"]] %>% unique() %>%
  purrr::map(~datapackcommons::getMetadata(base_url, 
                                           "dataElements", 
                                           glue::glue("id:eq:{.x}")))


denominator_components <- indicators$denominator[indicators$denominator != 1] %>% 
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() %>% .[["data_element_uid"]] %>% unique() %>%
  purrr::map(~datapackcommons::getMetadata(base_url, 
                                           "dataElements", 
                                           glue::glue("id:eq:{.x}")))

combined_componenets <- dplyr::bind_rows(numerator_components, denominator_components) %>% 
  distinct()

de_19r_20t <- c(fy_20_t$dataelement,fy_19_r$dataelement) %>% unique()

dplyr::setdiff(combined_componenets$id, de_19r_20t)






