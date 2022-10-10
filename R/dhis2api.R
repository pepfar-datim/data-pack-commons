#' @export
#' @title ValidateNameIdPairs(names, ids, type)
#' @importFrom datimutils %.in%
#'
#' @description Checks name list and paired id list (same length) and verifies they correspond to each other
#' @param names string vector - names of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @param exact boolean - exact = true matches the full name in datim with the name provided
#' exact = false is a case sensitive serch that the name provided is part of the name in datim
#' @param d2_session
#' @return  dplyr::all_equal response for exact = true or tibble of mismatches if exact = false
#'
ValidateNameIdPairs <- function(names, ids, type, exact = TRUE,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  # TODO the exact and inexact paths were written at different times for different purposes,
  # harmonize the return format
  assertthat::assert_that(is.character(names), assertthat::not_empty(names), NCOL(names) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(names) == length(ids))
  original <- tibble::tibble(name = names, id = ids) %>% unique()
  ids_csv  <-  unique(ids) %>%
    paste0(collapse = ",")
  response <- datimutils::getMetadata(!!type,
                                      filters = id %.in% ids_csv,
                                      fields = "id,name",
                                      d2_session = d2_session)
  assertthat::has_name(response, "name")
  assertthat::has_name(response, "id")
  if (exact == TRUE) {
    result <- dplyr::all_equal(original, response)
    if (result != TRUE) {
      stop(list(result = result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
    }
  } else {
    mismatched  <-  dplyr::left_join(original, response, by = c("id" = "id")) %>%
             dplyr::mutate(match = stringr::str_detect(name.y, name.x)) %>%
             dplyr::filter(match == FALSE | is.na(match))
    if (NROW(mismatched) > 0) {
      return(mismatched %>%
               dplyr::rename("name" = "name.x", "name_datim" = "name.y"))
    }
    }
  TRUE
  }

#' @export
#' @title ValidateCodeIdPairs
#'
#' @description Checks code list and paired id list (same length) and verifies they correspond to each other
#' @param codes string vector - code of spcific class of metadata - category option, indicator etc
#' @param ids string vector - ids of specific class of metadata - category option, indicator etc
#' @param type string - metadata endpoint - cataegoryOptions, indicators, etc
#' @param d2_session
#' @return  dplyr::all_equal response
#'
ValidateCodeIdPairs <- function(codes, ids, type,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  assertthat::assert_that(is.character(codes), assertthat::not_empty(codes), NCOL(codes) == 1,
                          is.character(ids),   assertthat::not_empty(ids),   NCOL(ids)   == 1,
                          assertthat::is.string(type),
                          length(codes) == length(ids))
  original <- tibble::tibble(code = codes, id = ids) %>%
    unique()
  ids_csv <-  ids %>%
    unique() %>%
    paste0(collapse = ",")
  response <- datimutils::getMetadata(!!type,
                                      filters = id %.in% ids_csv,
                                      fields = "id,code",
                                      d2_session = d2_session)
  assertthat::has_name(response, "code")
  assertthat::has_name(response, "id")
  result <-  dplyr::all_equal(original, response)
  if (result != TRUE) {
    stop(list(result = result, dplyr::anti_join(original, response), dplyr::anti_join(response, original)))
  } else {
    TRUE
  }
}

#' @export
#' @title GetData_DataPack
#' @param parameters paramemters for calling an indicator
#' from datapackcommons::data_required
#' @param  country uid
#' or FALSE if no military org_unit (FALSE for Philippines in COP20)
#' @param dim_item_sets datapackcommons::dim_item_sets or a subset
#' @param d2_session
#' @return  A list with $time = time the function was called,
#' $api_call = api call used, and
#' $results = the data returnd by the analytics call
#'
# indicator_parameters <- datapackcommons::StackPrefixedCols(data_required, c("A.", "B.")) %>%
#   unique() %>%
#   filter(!is.na(dx_id))
#  parameters = slice(indicator_parameters, 1)
# # dim_item_sets = datapackcommons::dim_item_sets
#  org_units= "XtxUYCsDWrR"
# # org_unit_levels=NULL
#  GetData_DataPack(parameters=parameters, org_units = org_units)

GetData_DataPack <- function(parameters,
                             org_units,
                             dim_item_sets = datapackcommons::dim_item_sets,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
  # This function processes only a single set of indicator paramaters
  # which are recieved in a single rowed tibble
  assertthat::assert_that(NROW(parameters) == 1)

  # Check names of columns in parameter tibble are as expected
  assertthat::are_equal(
    names(parameters),
    c(
      "custom_ou",
      "dx_name",
      "dx_id",
      "pe_iso",
      "age_set",
      "sex_set",
      "kp_set",
      "other_disagg_set",
      "technical_area",
      "technical_area_uid",
      "num_or_den",
      "num_or_den_uid",
      "disagg_type",
      "disagg_type_uid",
      "value_na"
    )
  )

  # data and period parameters
  # create the initial inputs for dimensions and period
  analytics_input <- list()
  analytics_input$dx <- parameters$dx_id[[1]]
  analytics_input$pe <- parameters$pe_iso[[1]]

  # add analytics filters for support type in DSD or TA
  # expect for indicators/data elements with no support type (AGYW, Priortization)
  if (!(parameters$dx_id %in% c("zPTqc4v5GAK", # FY21 Results AGYW_PREV Total D
                                "r4zbW3owX9n"))) {
    #IMPATT.PRIORITY_SNU (N, SUBNAT) TARGET:
    fils_list <- TWXpUVE2MqL %.f% c("cRAGKdWIDn4", "iM13vdNLWKb")
    analytics_input <-  append(analytics_input, fils_list)

  }

  # add rows to dimensions for org units
  # this technically allows for multiple org units, but practically
  # I think this function is only called with 1 OU
  # TODO simplify to assume only 1 OU/country uid which matches documentation
  # block
  if (!is.null(org_units)) {
    analytics_input$ou <- org_units
  }

  # add dimensions for the standard age, sex, kp, and other disaggregations
  # this gets translated into a list object via translateDims in order to leverage getAnalytics
  dimension_disaggs <-
    dim_item_sets %>%
    dplyr::mutate(type = "dimension") %>%
    dplyr::filter(
      model_sets %in% c(
        parameters$age_set,
        parameters$sex_set,
        parameters$kp_set,
        parameters$other_disagg_set
      )
    ) %>%
    dplyr::select(type, dim_item_uid, dim_uid) %>%
    unique()  %>%
    stats::na.omit() %>%
    translateDims() # there are some items in dim item sets with no source dimension
  # these are cases when a historic disaggregation doesn't exist
  # and we need to create the disaggregation allocation for the DataPack

  # prepare final analytics input
  analytics_input$timeout <- 300 # set timeout to over 5 minutes
  analytics_input$retry <- 3
  analytics_input_base <- append(analytics_input, dimension_disaggs)

  # custom data ----
  # Implemented for dreams SNUs for AGYW_PREV
  # currently used to select DREAMS SNU ou_group
  # we get an early return in this if block as later code
  # assumes standard orgunit groups of PSNU and Mil
  if (!is.na(parameters$custom_ou)) {

    # create custom input
    analytics_input_cus <- analytics_input_base

    # add custom ou dimension
    analytics_input_cus$ou <- c(analytics_input_cus$ou, parameters$custom_ou)

    results_custom <-
      try({
        do.call(datimutils::getAnalytics
                   , analytics_input_cus) %>%
          tibble()
        },
          silent = TRUE)

    if (is.error(results_custom) ||
       is.null(results_custom) ||
       NROW(results_custom) < 1) { # nothing to return

      api_call <- NULL
      results <- NULL
    } else {

      results <- results_custom
    }

    return(list("api_call" = "Not Avaialble...",
                "time" = lubridate::now("UTC"),
                "results" = results))
  }

  # base data starts without military ----
  # due to the existence of some countries planning at country level and
  # containing a military org unit below country level
  # we must pull data from PSNUs and Mil separately to avoid double counting
  # military data

  # create list of non-military Type of organisational unit
  non_mil_types_of_org_units <-
    datimutils::getDimensions("mINJi7rR1a6",
                              fields = "items[name,id]") %>%
    dplyr::filter(name != "Military") %>%
    .[["id"]]

  # add extra filters from non_mil_types
  f_extra <- toString(sprintf("'%s'", non_mil_types_of_org_units))
  filters_extra <- paste("mINJi7rR1a6", "%.f%", "c(", f_extra, ")")
  fils_list_extra <- eval(parse(text = filters_extra))

  # add extra dimension for COP Prioritization SNU
  analytics_input_non_mil <- analytics_input_base # create a copy for non mil call
  analytics_input_non_mil$ou <- c(analytics_input_non_mil$ou, "OU_GROUP-AVy8gJXym2D")
  analytics_input_non_mil <- append(analytics_input_non_mil, fils_list_extra)

  # get non-military (PSNU) data
      results_psnu <-
        do.call(datimutils::getAnalytics,
                analytics_input_non_mil
        ) %>%
        tibble()

  # military data added if needed ----
  # all OUs have military below the country level as standard
  # so a call for military data is always executed
  results_mil <- NULL

  # create military input
  analytics_input_mil <- analytics_input_base

  # add military ou dimension
  analytics_input_mil$ou <- c(analytics_input_mil$ou, "OU_GROUP-nwQbMeALRjL")

  # call military data
  results_mil <-
    do.call(datimutils::getAnalytics,
            analytics_input_mil) %>%
    tibble()

  # finalize results ----
  if (NROW(results_psnu) == 0 && NROW(results_mil) == 0) {
    # nothing to return
    results <- NULL
  } else if (NROW(results_mil) == 0) {
    # psnu but no mil data
    results <- results_psnu
  } else {
    # return everything
    results <- dplyr::bind_rows(results_psnu, results_mil)
  }

  return(list("api_call" = "Not Avaialble...",
              "time" = lubridate::now("UTC"),
              "results" = results))

}
