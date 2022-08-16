library(magrittr)
library(tidyverse)
require(foreach)
library(datimutils)

get_indicator_details <- function(uid) {
  datapackcommons::getMetadata("indicators",
                               glue::glue("id:eq:{uid}"),
                               "name, id, numerator, denominator")
}

getFormDetails <- function(fiscal_yyyy_int, stream) {
  datapackr::getDatasetUids(fiscal_yyyy_int, stream) %>%
    purrr::map(~datapackcommons::GetSqlView("DotdxKrNZxG",
                                            c("dataSets"),
                                            c(.x))) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-dataset) %>%
    distinct()
}

parseIndicators <- function(indicator_uids) {
  indicator_parts <- datimutils::getIndicators(indicator_uids,
                                               fields = "id,numerator,denominator") %>%
    dplyr::mutate(numerator_addends =
                    stringr::str_extract_all(numerator,
                                             "#\\{[a-zA-Z0-9]{11}([a-zA-Z0-9\\.]{12})?\\}"),
                  numerator_addends = purrr::map(numerator_addends,
                                                 stringr::str_remove_all, "#\\{"),
                  numerator_addends = purrr::map(numerator_addends,
                                                 stringr::str_remove_all, "\\}"),
                  numerator_addends_de = purrr::map(numerator_addends,
                                                    stringr::str_sub, 1, 11),
                  numerator_addends_coc = purrr::map(numerator_addends,
                                                     stringr::str_sub, 13, 23),
                  numerator_addends_coc = purrr::map(numerator_addends_coc,
                                                     na_if, ""),
                  denominator_addends =
                    stringr::str_extract_all(denominator,
                                             "#\\{[a-zA-Z0-9]{11}([a-zA-Z0-9\\.]{12})?\\}"),
                  denominator_addends = purrr::map(denominator_addends,
                                                   stringr::str_remove_all, "#\\{"),
                  denominator_addends = purrr::map(denominator_addends,
                                                   stringr::str_remove_all, "\\}"),
                  denominator_addends_de = purrr::map(denominator_addends,
                                                      stringr::str_sub, 1, 11),
                  denominator_addends_coc = purrr::map(denominator_addends,
                                                       stringr::str_sub, 13, 23),
                  denominator_addends_coc = purrr::map(denominator_addends_coc,
                                                       na_if, "")
    )


  return(indicator_parts)
}


doMC::registerDoMC(cores = 5)

datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "datim.json"))

data_required <- datapackcommons::data_required


# get data element and category option combos

fy_20_r  <-  getFormDetails(2020, "mer_results") %>%
  dplyr::mutate(de.coc = paste(dataelementuid,
                               categoryoptioncombouid,
                               sep = "."),
                fy_20_r = TRUE)
fy_21_r  <-  getFormDetails(2021, "mer_results") %>%
  dplyr::mutate(de.coc = paste(dataelementuid,
                               categoryoptioncombouid,
                               sep = "."),
                fy_21_r = TRUE)
fy_21_t  <-  getFormDetails(2021, "mer_targets") %>%
  dplyr::mutate(de.coc = paste(dataelementuid,
                               categoryoptioncombouid,
                               sep = "."),
                fy_21_t = TRUE)
fy_22_t  <-  getFormDetails(2022, "mer_targets") %>%
  dplyr::mutate(de.coc = paste(dataelementuid,
                               categoryoptioncombouid,
                               sep = "."),
                fy_22_t = TRUE)

fy_23_t  <-  getFormDetails(2023, "mer_targets") %>%
  dplyr::mutate(de.coc = paste(dataelementuid,
                               categoryoptioncombouid,
                               sep = "."),
                fy_23_t = TRUE)

fy_22_t_fy_21_r_de_coc <- c(fy_22_t$dataelementuid,
                            fy_21_r$dataelementuid,
                            fy_22_t$de.coc,
                            fy_21_r$de.coc) %>% unique()

fy_21_r_to_fy_20_r_diff <- dplyr::full_join(fy_20_r, fy_21_r) %>%
  dplyr::arrange() %>%
  dplyr::filter(is.na(fy_21_r) | is.na(fy_20_r))

fy_22_t_to_fy_21_t_diff <- dplyr::full_join(fy_21_t, fy_22_t) %>%
  dplyr::arrange() %>%
  dplyr::filter(is.na(fy_21_t) | is.na(fy_22_t))

fy_23_t_to_fy_22_t_diff <- dplyr::full_join(fy_22_t, fy_23_t) %>%
  dplyr::arrange() %>%
  dplyr::filter(is.na(fy_22_t) | is.na(fy_23_t))

####################  stopped here


indicators_required <-
  c(data_required$A.dx_id, data_required$B.dx_id) %>%
  unique() %>%
  na.omit() %>%
  parseIndicators() %>%
  dplyr::mutate(matching_num_elements =
                  purrr::map(numerator_addends,
                             match,
                             fy_22_t_fy_21_r_de_coc),
                matching_den_elements =
                  purrr::map(denominator_addends,
                             match,
                             fy_22_t_fy_21_r_de_coc),
                missing_any_elements =
                  (purrr::map2_lgl(matching_num_elements,
                               matching_num_elements,
                    ~any(is.na(.x),
                         is.na(.y)))))

data_elements_only_missing <- dplyr::setdiff(c(data_required$A.dx_id,
                                           data_required$B.dx_id),
                                         indicators_required$id) %>%
  dplyr::setdiff(fy_21_t_fy_20_r_de_coc)


print(paste("These data elements are referenced but not available",
            datimutils::getDataElements(data_elements_only_missing)))

indicators_w_missing_elements <-
  dplyr::filter(indicators_required,
                missing_any_elements)

purrr::map(indicators_required$denominator_addends,
           match, fy_21_t_fy_20_r_de_coc)

de_coc <- c(indicators_required$numerator_addends,
            indicators_required$denominator_addends) %>%
  unlist() %>%
  unique()



#FY22 Update stopped here

extract_formula_components <- function(formula) {
  stringr::str_split(formula, "[+-]")[[1]] %>%
    stringr::str_remove_all(" ") %>%
    stringr::str_remove_all("\\#") %>%
    stringr::str_remove_all("\\{") %>%
    stringr::str_remove_all("\\}") %>%
    stringr::str_remove_all("\\(") %>%
    stringr::str_remove_all("\\)") %>%
    tibble::enframe(name = NULL) %>%
    dplyr::mutate(data_element_uid = stringr::str_sub(value, 1, 11)) %>%
    dplyr::mutate(co_combination_uid = dplyr::if_else(stringr::str_length(value) > 11,
                                                      stringr::str_sub(value, 13, 23),
                                                      NA_character_)) %>%
    dplyr::select(- value)
}

elements_fy19r_fy20t <- dplyr::bind_rows(fy_19_r, fy_20_t) %>% dplyr::distinct()


# -------------------------------------------------------------------------
compare_indicator_cocs <- function(indicator, dataset_details) {

  numerator_components <- indicator$numerator %>%
    extract_formula_components() %>%
    dplyr::bind_rows() %>%
    na.omit() %>%
    dplyr::mutate(dataelementuid = data_element_uid,
                  categoryoptioncombouid = co_combination_uid)

  numerator_component_des <- numerator_components$data_element_uid %>% unique()
  numerator_ind_to_dataset <- dataset_details %>%
    dplyr::filter(dataelementuid %in% numerator_component_des) %>%
    dplyr::full_join(numerator_components)

  denominator_components <<- indicator$denominator %>%
    extract_formula_components() %>%
    dplyr::bind_rows() %>%
    na.omit() %>%
    dplyr::mutate(dataelementuid = data_element_uid,
                  categoryoptioncombouid = co_combination_uid)

  denominator_component_des <- denominator_components$data_element_uid %>% unique()
  denominator_ind_to_dataset <- dataset_details %>%
    dplyr::filter(dataelementuid %in% denominator_component_des) %>%
    dplyr::full_join(denominator_components)

  return(list(numerator_ind_to_dataset = numerator_ind_to_dataset,
              denominator_ind_to_dataset = denominator_ind_to_dataset))
}

get_invalid_data_elements <- function(data_required_spec) {
  print(data_required_spec)
indicators  <-  c(data_required_spec$A.dx_id,
                  data_required_spec$B.dx_id) %>%
  unique() %>%
  na.omit() %>%
  purrr::map(get_indicator_details) %>%
  purrr::compact() %>%
  dplyr::bind_rows()

if (length(indicators) == 0) {
  return(NULL)
}

numerator_components <- indicators$numerator %>%
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows()

numerator_data_elements <- numerator_components %>%
  .[["data_element_uid"]] %>%
  unique() %>%
  purrr::map(~datapackcommons::getMetadata("dataElements",
                                           glue::glue("id:eq:{.x}"))) %>%
  dplyr::bind_rows()


denominator_components <- indicators$denominator[indicators$denominator != 1] %>%
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows()

denominator_data_elements <- denominator_components %>%
  .[["data_element_uid"]] %>%
  unique() %>%
  purrr::map(~datapackcommons::getMetadata("dataElements",
                                           glue::glue("id:eq:{.x}"))) %>%
  dplyr::bind_rows()

combined_data_elements <- dplyr::bind_rows(numerator_data_elements, denominator_data_elements)

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
                                           stringr::str_detect(numerator, "\\.") |
                                             stringr::str_detect(denominator, "\\."))


temp <- plyr::alply(indicators_with_co_combos, 1,
                    compare_indicator_cocs, elements_fy19r_fy20t) %>%
  set_names(indicators_with_co_combos$name)





numerator_components <- indicators$numerator %>%
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() %>%
  .[["data_element_uid"]] %>%
  unique() %>%
  purrr::map(~datapackcommons::getMetadata("dataElements",
                                           glue::glue("id:eq:{.x}")))


denominator_components <- indicators$denominator[indicators$denominator != 1] %>%
  purrr::map(extract_formula_components) %>%
  dplyr::bind_rows() %>%
  .[["data_element_uid"]] %>%
  unique() %>%
  purrr::map(~datapackcommons::getMetadata("dataElements",
                                           glue::glue("id:eq:{.x}")))

combined_componenets <- dplyr::bind_rows(numerator_components, denominator_components) %>%
  distinct()

de_19r_20t <- c(fy_20_t$dataelement, fy_19_r$dataelement) %>% unique()

dplyr::setdiff(combined_componenets$id, de_19r_20t)





# vmmc_circ yield
fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Positive") |
                  stringr::str_detect(categoryoptioncombo, "Negative")) %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")


fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Positive")) %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")


# vmmc_circ indeterminate

fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "VMMC_CIRC \\(N, ")) %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/HIVStatus")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Unknown")) %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")


# ovc_serv target

temp <- fy_20_t %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "OVC_SERV"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")

# tx_tb_d results already

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Already"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Already"))  %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "TB Screen - Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")


# tx_tb_d results new

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "New"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TX_TB \\(D"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/TBScreen/NewExistingART/")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "New"))  %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "TB Screen - Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")



# pmtct_stat d Percentage at Entry Known Positive

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Known at Entry Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")



# PMTCT_STAT.N.newYield denominator

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive") |
                  stringr::str_detect(categoryoptioncombo, "Newly Identified Negative"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")

# PMTCT_STAT.N.newYield numerator

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "PMTCT_STAT \\(N"))  %>%
  dplyr::filter(stringr::str_detect(dataelement, "Age/Sex/KnownNewResult")) %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")


# TB_STAT.N.newYield denominator

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TB_STAT \\(N"))  %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Newly Identified Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")

# TB_STAT Percentage With already known status numerator

temp <- fy_19_r %>%
  dplyr::select(-dataset) %>%
  distinct() %>%
  dplyr::filter(stringr::str_detect(dataelement, "TB_STAT \\(N"))  %>%
  dplyr::filter(stringr::str_detect(categoryoptioncombo, "Known at Entry Positive"))  %>%
  dplyr::transmute(element = paste0("#{", dataelementuid, ".", categoryoptioncombouid, "}")) %>%
  .[["element"]] %>%
  glue::glue_collapse(" + ")
