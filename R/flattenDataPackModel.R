#' @export
#' @title flattenDataPackModel_21
#'
#' @description
#' Takes a COP 21 Data Pack Model data file and removes non-results content and flattens
#' the results hierarchy
#'
#' @param data list object with COP 21 model data
#' @param country_uids string vector listing country uids at the top level of the list hierarchy.
#'
#' @return list object with one result set for each country named by country uid
flattenDataPackModel_21 <- function(data, country_uids = NULL) {

  flattenCountry <- function(data) { # takes country level list and flattens sheet level
    sheets <- c("AGYW", "PMTCT_STAT_ART", "PMTCT_EID", "TB_STAT_ART",
               "VMMC", "TX", "HTS", "TB_TX_PREV",
               "OVC", "CXCA", "KP", "PP", "PrEP",
               "GEND", "Prioritization", "Cascade")
    purrr::map(sheets, ~flattenSheet(data[[.x]])) %>%
      dplyr::bind_rows() %>%
      dplyr::rename(psnu_uid = org_unit_uid) %>%
      dplyr::select(indicator_code,
                    period,
                    psnu_uid,
                    age_option_uid,
                    sex_option_uid,
                    kp_option_uid,
                    value)
  }

  flattenSheet <- function(data) { # takes sheet level list, extracts results data frames,
    # adds indicator_code column and binds them
    indicators <- names(data)
    purrr::map(indicators, ~ data[[.x]][["results"]] %>%
                 dplyr::mutate(indicator_code = .x)) %>%
      dplyr::bind_rows()
  }

  if (is.null(country_uids)) {
    # If no country list is provided - flatten them all

    country_uids <- names(data)
  } else {
    # make sure all the country uids provided are valid (e.g. part of the input file)
    if (!all(country_uids %in% names(data))) {
      stop("One or more of the country uids is not contained in the data file")
    }
    # filter down to the relevant subset of countries
    data <- data[country_uids]
  }

  purrr::map(country_uids, ~flattenCountry(data[[.x]])) %>% setNames(country_uids)
}
