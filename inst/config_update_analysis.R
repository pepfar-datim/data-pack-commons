library(tidyverse)
require(foreach)
doMC::registerDoMC(cores = 5)

datapackcommons::DHISLogin("/users/sam/.secrets/jason.json")
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
    stringr::str_remove_all("\\}") %>%
    stringr::str_remove_all("\\(") %>% 
    stringr::str_remove_all("\\)") %>%
    tibble::enframe(name=NULL) %>% 
    dplyr::mutate(data_element_uid = stringr::str_sub(value, 1,11)) %>% 
    dplyr::mutate(co_combination_uid = dplyr::if_else(stringr::str_length(value) > 11 ,
                                                      stringr::str_sub(value, 13,23),
                                                      NA_character_)) %>% 
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
compare_indicator_cocs <- function(indicator, dataset_details){
  
  numerator_components <- indicator$numerator %>% 
    extract_formula_components() %>%
    dplyr::bind_rows() %>% na.omit() %>% dplyr::mutate(dataelementuid = data_element_uid,
                                                       categoryoptioncombouid = co_combination_uid)
  
  numerator_component_des <- numerator_components$data_element_uid %>% unique()
  numerator_ind_to_dataset <- dataset_details %>% dplyr::filter(dataelementuid %in% numerator_component_des) %>% 
    dplyr::full_join(numerator_components)
  
  denominator_components <<- indicator$denominator %>% 
    extract_formula_components() %>%
    dplyr::bind_rows() %>% na.omit() %>% dplyr::mutate(dataelementuid = data_element_uid,
                                                       categoryoptioncombouid = co_combination_uid)
  
  denominator_component_des <- denominator_components$data_element_uid %>% unique()
  denominator_ind_to_dataset <- dataset_details %>% dplyr::filter(dataelementuid %in% denominator_component_des) %>% 
    dplyr::full_join(denominator_components)
  
  return(list(numerator_ind_to_dataset = numerator_ind_to_dataset, 
              denominator_ind_to_dataset = denominator_ind_to_dataset))
}

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






plyr::alply(data_required, 1, get_invalid_data_elements, .parallel = TRUE)







indicators  <-  c(data_required$A.dx_id,
                  data_required$B.dx_id) %>% 
  unique() %>% 
  na.omit() %>% 
  purrr::map(get_indicator_details) %>% 
  purrr::compact() %>% 
  dplyr::bind_rows()

indicators_with_co_combos <- dplyr::filter(indicators,
                                           stringr::str_detect(numerator,"\\.") |
                                             stringr::str_detect(denominator,"\\."))


temp = plyr::alply(indicators_with_co_combos, 1, 
                   compare_indicator_cocs, elements_fy19r_fy20t) %>% set_names(indicators_with_co_combos$name)





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





# vmmc_circ yield
fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Positive") |
                  stringr::str_detect(categoryoptioncombo, "Negative")) %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")


fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Positive")) %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")


# vmmc_circ indeterminate

fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Unknown")) %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")


# ovc_serv target

temp = fy_20_t %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "OVC_SERV"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")

# tx_tb_d results already

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/"))%>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Already"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/"))%>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Already"))  %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "TB Screen - Positive"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")


# tx_tb_d results new

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/"))%>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "New"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/"))%>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "New"))  %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "TB Screen - Positive"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")



# pmtct_stat d Percentage at Entry Known Positive

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Known at Entry Positive"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")



# PMTCT_STAT.N.newYield denominator

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive") |
                  stringr::str_detect(categoryoptioncombo, "Newly Identified Negative"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")

# PMTCT_STAT.N.newYield numerator

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>% 
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")


# TB_STAT.N.newYield denominator

temp = fy_19_r %>% 
  dplyr::select(-dataset) %>% distinct() %>% 
  dplyr::filter(stringr::str_detect(dataelement, "TB_STAT \\(N"))  %>% 
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive"))  %>% 
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>% 
  glue::glue_collapse(" + ")