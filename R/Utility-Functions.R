#' @export
#' @title Translate Dimensions
#'
#' @description takes a dimensions data frame and translates it into a list item for input to get_analytics
#' @param dimensions_df a dimensions data frame.
#' @return  List of dimensions for the analytics call to datimutils::getAnalytics
#'
translateDims <- function(dimensions_df) {

  dims <- dimensions_df %>% select(dim_uid) %>% unique()

  res <- dims$dim_uid %>% lapply(function(uid) {
    # prepare dim item uids and dim
    dim_uid <- sprintf("'%s'", uid)
    dim_item_uids <- toString(sprintf("'%s'", dimensions_df[dimensions_df$dim_uid == uid, ]$dim_item_uid))

    # prepare param to pass
    # we pre-evaluate so that the api params are set for passing
    res <- paste(dim_uid, "%.d%", "c(", dim_item_uids, ")")
    res <- eval(parse(text = res))

  })
  return(res)
}

#' @export
#' @title Translate Filters
#'
#' @description takes a filters data frame and translates it into a list item for input to get_analytics
#' @param filters_df a dimensions data frame.
#' @return  List of dimensions for the analytics call to datimutils::getAnalytics
#'
translateFils <- function(dimensions_df) {

  dims <- dimensions_df %>% select(dim_uid) %>% unique()

  res <- dims$dim_uid %>% lapply(function(uid) {
    # prepare dim item uids and dim
    dim_uid <- sprintf("'%s'", uid)
    dim_item_uids <- toString(sprintf("'%s'", dimensions_df[dimensions_df$dim_uid == uid, ]$dim_item_uid))

    # prepare param to pass
    # we pre-evaluate so that the api params are set for passing
    res <- paste(dim_uid, "%.f%", "c(", dim_item_uids, ")")
    res <- eval(parse(text = res))

  })
  return(res)
}

#' @export
#' @title StackPrefixedCols(data, prefixes)
#'
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data.
#' @param data dataframe - contains data to stack
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
StackPrefixedCols <- function(data, prefixes) {
  assertthat::assert_that(length(prefixes) > 0, is.data.frame(data))
  SelectAndStripPrefix <- function(prefix, df) {
    dplyr::select(df, dplyr::starts_with(prefix, ignore.case = FALSE)) %>%
      dplyr::rename_all(.funs = stringr::str_remove, pattern = prefix)
  }

  purrr::map(prefixes, SelectAndStripPrefix, data) %>% dplyr::bind_rows()
}

#' @export
#' @title FormatForApi_Dimensions(data, type_col, dim_id_col, item_id_col)
#'
#' @description Uses specified columns in a data from to produce APIrequest
#' formated dimensions e.g. &dimension=dim-id:dim-item;dim-item
#' Only includes unique dimension, dim-id, dim-item tupples
#' @param data dataframe - containing parameters to incorporate into api call
#' @param type_col string - name of column in data that specifies "dimension"
#' or "filter"
#' @param dim_id_col string - name of column in data that specifies
#' dimension ids - including dx, ou, etc.
#' @param item_id_col string - name of column in data that specifies
#' dimension item ids
#' @return  string ready for api call such as
#' "dimension=dim-id:dim-item;dim-item&filter=dim-id:dim-item;dim-item"
#' Note there is no leading "&" in string
#' @examples
#' df = tibble::tribble(~type, ~dim_id, ~item_id, ~other_col,
#' "dimension",    "LFsZ8v5v7rq", "CW81uF03hvV",
#' "Implementing Partner: AIDSRelief Consortium",
#' "dimension",    "LFsZ8v5v7rq", "C6nZpLKjEJr",
#' "Implementing Partner: African Medical and Research Foundation",
#' "filter", "dx", "BOSZApCrBni", "ART enrollment stage 1",
#' "filter", "dx", "dGdeotKpRed", "ART enrollment stage 2",
#' "dimension", "ou", "O6uvpzGd5pu", "Bo",
#' "filter", "pe", "THIS_FINANCIAL_YEAR","")
#' FormatForApi_Dimensions(df, "type", "dim_id", "item_id")
#'
FormatForApi_Dimensions <- function(data, type_col, dim_id_col, item_id_col) {
  assertthat::assert_that(assertthat::has_name(data, type_col),
                          assertthat::has_name(data, dim_id_col),
                          assertthat::has_name(data, item_id_col))
  data %>%
    dplyr::mutate(type = data[[type_col]],
                  dim_id = data[[dim_id_col]],
                  item_id = data[[item_id_col]])  %>%
    dplyr::select(type, dim_id, item_id) %>%
    unique() %>%
    dplyr::group_by_at(c("type", "dim_id"))  %>%
    dplyr::summarise(items = paste0(item_id, collapse = ";")) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(component = glue::glue("{type}={dim_id}:{items}")) %>%
    .[[1]] %>%
    paste0(collapse = "&")
}

#' @export
#' @title RenameDimensionColumns(data, type)
#'
#' @description Renames the original column names of datapackcommons::dim_items_sets,
#' by prepending the string in the type parameter
#' @param data the unique dim_cop_type that is passed in the MapDimToOptions method
#' @param type It will pre-pend the string in type to the columns names
#' @return  The dataframe with renamed column names for dimensions
#'
RenameDimensionColumns <- function(data, type) {
  data %>%
    dplyr::rename(!!paste0(type, "_dim_uid") := dim_uid,
                  !!paste0(type, "_dim_name") := dim_name,
                  !!paste0(type, "_dim_cop_type") := dim_cop_type,
                  !!paste0(type, "_dim_item_name") := dim_item_name,
                  !!paste0(type, "_option_name") := option_name,
                  !!paste0(type, "_option_uid") := option_uid,
                  !!paste0(type, "_sort_order") := sort_order,
                  !!paste0(type, "_weight") := weight,
                  !!paste0(type, "_model_sets") := model_sets) %>%
    return()
}

#' @export
#' @title MapDimToOptions(data, items_to_options, allocate)
#'
#' @description A function that maps dimensions from a dataframe to the options sets
#' @param data dataframe - dimension name and dimension UID, along with the quantity
#' @param items_to_options dimension item sets dataframe filtered by one of the model sets
#' @param allocate If allocate is set to "distriute", mutates a column in the returned df with
#' the weight being multiplied to the value
#' @return If there are no options provided, returns the analytics output, else if there are no dim_uid
#' in the options list, joins the data using crossing or left join,
#' else if the allocation is set to "distriute", then renames them adds a value column and
#' finally performs the renaming of the dimension columns.
#'
MapDimToOptions <- function(data, items_to_options, allocate) {

  if (NROW(items_to_options) == 0) {
    return(data)
  }

  dimension_uid <- unique(items_to_options$dim_uid)
  cop_category <- unique(items_to_options$dim_cop_type)
  assertthat::assert_that(NROW(dimension_uid) == 1, NROW(cop_category) == 1)

  if (is.na(dimension_uid)) {
    # We are in a scenario of distributing to category options in the absence of a source dimension
    # so we need cartesian product of data with item_to_dim entries
    joined_data <- tidyr::crossing(data,
                                   dplyr::select(items_to_options, -dim_item_uid))
  } else {
    dim_name <-  items_to_options[[1, "dim_name"]]
    joined_data <- data %>%
      dplyr::left_join(items_to_options, by = stats::setNames("dim_item_uid", dim_name))
  }

  if (allocate == "distribute") {
    joined_data %>%
      dplyr::mutate(Value = Value * weight) %>%
      RenameDimensionColumns(cop_category)
  } else {
    joined_data %>%
      RenameDimensionColumns(cop_category)
  }
}

#' @export
#' @title diffSnuximModels(model_old, model_new, full_diff = TRUE)
#'
#' @description A function that compares old and new snuxim models
#' @param model_old list - snuxim model
#' @param model_new list - snuxim model
#' @param full_diff If TRUE compares model difference fully
#' @return deltas representing difference in data of both models
#'
diffSnuximModels <- function(model_old,
                             model_new,
                             full_diff = TRUE,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)
) {

  # only include countries present in both files
  if (full_diff) {
    country_details <- dplyr::union(names(model_old),
                                    names(model_new))
  } else {
    country_details <- dplyr::intersect(names(model_old),
                                        names(model_new))
  }

  # bind list item rows and add relevant columns
  model_old_filtered <- dplyr::bind_rows(model_old[country_details]) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = round(value, 5),
                  percent = round(percent, 5)) %>%
    rename(value.old = value,
           percent.old = percent)
  #dplyr::mutate(value = round(value, 2)) %>%
  #dplyr::mutate(old = 1)

  model_new_filtered <- dplyr::bind_rows(model_new[country_details]) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(value = round(value, 5),
                  percent = round(percent, 5)) %>%
    rename(value.new = value,
           percent.new = percent)
  #dplyr::mutate(value = round(value, 2)) %>%
  #dplyr::mutate(old = 1)
  deltas  <-  dplyr::full_join(model_new_filtered, model_old_filtered) %>%
    dplyr::filter(value.new != value.old |
                    percent.new != percent.old |
                    is.na(value.new) |
                    is.na(value.old) |
                    is.na(percent.new) |
                    is.na(percent.old)
    )

  # add other columns
  ancestors <- lapply(
    datimutils::getOrgUnits(deltas$psnu_uid,
                            fields = "ancestors[name]",
                            d2_session = d2_session),
    tibble::as_tibble
  )

  deltas <- dplyr::mutate(deltas,
                          psnu = datimutils::getOrgUnits(deltas$psnu_uid,
                                                         d2_session = d2_session),
                          ou = purrr::map_chr(ancestors, purrr::pluck, 1, 3),
                          snu1 = purrr::map_chr(ancestors, purrr::pluck, 1, 4, .default = NA_character_))

  return(deltas)
}

#' @export
#' @title diffDataEntryForm(uid_a, uid_b)
#'
#' @description A function that compares two data entry forms based on uid inputs
#' that access sqlviews
#' @param dataset_a a dataset uid
#' @param dataset_b a dataset uid
#' @param d2_session a d2_session
#' @return a list of all three differences
#'
diffDataEntryForm <- function(
    uid_a,
    uid_b,
    d2_session = dynGet("d2_default_session", inherits = TRUE)
) {


  # pull the sql views
  a <-  datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                             variable_keys = c("dataSets"),
                             variable_values = c(uid_a)) %>%
        dplyr::rename("A.dataset" = "dataset")


  b <- datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                variable_keys = c("dataSets"),
                                variable_values = c(uid_b)) %>%
      dplyr::rename("B.dataset" = "dataset")

  # run the diff
  res <- diffDataFrames(
    a,
    b
  )

  return(res)

}

#' @export
#' @title diffDataFrames(dataframe_a, dataframe_b)
#'
#' @description A function that compares two dataframes and returns a list
#' combination of joins between those two dataframes
#' @param dataset_a a dataframe
#' @param dataset_b a dataframe
#' @return a list of all three differences
#'
diffDataFrames <- function(
  dataframe_a,
  dataframe_b
  ) {

  # ensure data frames are present as params
  if (is.null(dataframe_a) || is.null(dataframe_b)) {
    stop("one or both of your dataframe values are empty!")
  }

  # ensure the class of the objects going in
  if (!is.data.frame(dataframe_a) || !is.data.frame(dataframe_b)) {
    stop("one or both of these are not dataframes!")
  }

  # ensure names of the data frames are the same
  # filter out dataset column so as not to trip error
  if (!identical(names(dataframe_a)[!grepl("dataset", names(dataframe_a))],
                names(dataframe_b)[!grepl("dataset", names(dataframe_b))])
     ) {
    stop("your dataframes seem to have different names!")
  }

  # in a but not b
  a_not_b <- tryCatch({
    dplyr::anti_join(dataframe_a, dataframe_b)
  }, error = function(e) {
    return(NULL)
  })

  # in b but not a
  b_not_a <- tryCatch({
    dplyr::anti_join(dataframe_b, dataframe_a)
  }, error = function(e) {
    return(NULL)
  })

  # in a and b
  a_and_b <- tryCatch({
    dplyr::inner_join(dataframe_a, dataframe_b)
  }, error = function(e) {
    print(e)
    return(NULL)
  })

  # list
  diff_list <-
    list(
      "a_not_b" = a_not_b,
      "b_not_a" = b_not_a,
      "a_and_b" = a_and_b
    )

  return(diff_list)

}

#' @export
#' @title createDissagReport(model, cop_year)
#'
#' @description creates a report showing disagg msimatches and other visual aids
#' @param model a datapack model
#' @param disagg a cop year
#' @return a list with 3 reports of different granularity on disagg mismatches
#'
createDissagReport <- function(model, cop_year) {

  print(paste0("report build for fy: ", cop_year))
  adj_model_fy <- max(as.numeric(substr(unique(model[!is.na(model$period),]$period), 1, 4))) + 1

  # check cop year is valid against the model
  if ( adj_model_fy != cop_year ) {
    stop("the model input does not match the cop year!")
  }

  # what are the valid datapack indicators
  if(cop_year == 2024) {
    valid_schema_indicators <-
      filter(datapackr::cop24_data_pack_schema,
             (dataset == "mer" & col_type == "past") |
               (dataset == "datapack" & col_type == "calculation")) %>%
      select(indicator_code, valid_ages, valid_sexes, valid_kps) %>%
      distinct()
  } else if(cop_year == 2023) {
    valid_schema_indicators <-
      filter(datapackr::cop23_data_pack_schema,
             (dataset == "mer" & col_type == "past") |
               (dataset == "datapack" & col_type == "calculation")) %>%
      select(indicator_code, valid_ages, valid_sexes, valid_kps) %>%
      distinct()
  } else {
    stop("cop year for schema supplied is not supported!")
  }

  # spread schema data visually
  valid_schema_indicators <-
    lapply(1:nrow(valid_schema_indicators), function(y) {

      valid_schema_indicators[y,] %>%
        mutate(
          valid_a = ifelse(isFALSE(all(is.na(valid_ages[[1]]))), paste(sort( valid_ages[[1]]$id ), collapse = ", "), NA),
          valid_s = ifelse(isFALSE(all(is.na(valid_sexes[[1]]))), paste(sort (valid_sexes[[1]]$id ), collapse = ", "), NA),
          valid_k = ifelse(isFALSE(all(is.na(valid_kps[[1]]))), trimws( paste(sort( valid_kps[[1]]$id ), collapse = ", ") ), NA)
        )

    }) %>% data.table::rbindlist()

  # join model data against the valid schema
  model_schema_joined <-
    model %>%
    group_by(indicator_code, age_option_uid, sex_option_uid,kp_option_uid) %>%
    summarise(value = sum(value)) %>%
    inner_join(
      valid_schema_indicators %>% select(indicator_code, valid_ages, valid_sexes, valid_kps, valid_a, valid_s, valid_k)
    ) %>%
    arrange(indicator_code)

  # label every match and mismatch vector
  model_schema_joined_e <-
    lapply(unique(model_schema_joined$indicator_code), function(y) {

      q = model_schema_joined %>% filter(indicator_code == y)

      u = q %>%
        mutate(model_a = paste( sort(unique(q$age_option_uid)), collapse = ", " ) ) %>%
        mutate(model_s = paste( sort(unique(q$sex_option_uid)), collapse = ", " ) ) %>%
        mutate(model_k = trimws (paste( sort(unique(q$kp_option_uid)), collapse = ", " ) ) ) %>%
        mutate(model_k = ifelse(model_k == "", NA, model_k)) %>%
        mutate(age_match = ifelse( identical(model_a, valid_a) | is.na(model_a == valid_a), TRUE, FALSE) ) %>%
        mutate(sex_match = ifelse( identical(model_s, valid_s) | is.na(model_s == valid_s), TRUE, FALSE) ) %>%
        mutate(kp_match = ifelse( identical(model_k, valid_k) | is.na(model_k == valid_k), TRUE, FALSE) )

    }) %>% data.table::rbindlist()

  # filter out the mismatched for visual check
  mismatched <- model_schema_joined_e %>% filter(age_match == FALSE | sex_match == FALSE | kp_match == FALSE) %>%
    distinct(indicator_code, .keep_all = T)

  # function for checking mismatch specifics
  checkDisagg <- function(model, schema, disagg) {

    # where there is a mismatch what is that mismatch?
    lapply(unique(model$indicator_code), function(indicator_c) {

      if( !disagg %in% c("age", "sex", "kp") ) {
        stop("you have not entered a valid kp!")
      }

      if(disagg == "age") {
        mod_id_val <- "age_option_uid"
        sch_id_val <- "valid_ages"
      } else if (disagg == "sex") {
        mod_id_val <- "sex_option_uid"
        sch_id_val <- "valid_sexes"
      } else {
        mod_id_val <- "kp_option_uid"
        sch_id_val <- "valid_kps"
      }

      # keep all model data for that indicator where there are values
      model <- model %>%
        filter(indicator_code == indicator_c)

      # keep the indicator data for the schema
      schema <-
        datapackr::cop24_data_pack_schema  %>%
        filter(indicator_code ==  indicator_c)

      # in schema not in model
      s <- dplyr::anti_join(
        schema[[sch_id_val]][[1]] %>% distinct(id) %>% arrange(),
        as_tibble(model) %>% arrange() %>% select(id = mod_id_val)
      )

      #print(indicator_c)
      #print(s)

      # in model not in schema
      m <- dplyr::anti_join(
        as_tibble(model) %>% arrange() %>% select(id = mod_id_val),
        schema[[sch_id_val]][[1]] %>% distinct(id) %>% arrange()
      )

      msg1 <- if(NROW(s) > 0) {
        paste0("MODEL MISSING: ", paste0(unique(s$id), collapse = ","))
      } else {
        paste0("")
      }

      msg2 <- if(NROW(m) > 0) {
        paste0("model has extra ", disagg , " disaggs: ", paste0(unique(m$id), collapse = ","))
      } else {
        paste0("")
      }

      msg <- trimws(paste(msg1, msg2))

      res_f <- tibble(
        indicator_code = indicator_c,
        disagg = disagg,
        #msg = list(msg),
        msg = msg
      )

    }) %>% data.table::rbindlist()

  }

  # loop through different dissag types to check match
  disagg_msgs <-
    lapply(c("age", "sex", "kp"), function(y){
      checkDisagg(model = model_schema_joined_e, schema = valid_schema_indicators, disagg = y)
    }) %>% data.table::rbindlist()

  # spread to merge
  disagg_msgs_w <-
    tidyr::pivot_wider(
      disagg_msgs,
      id_cols = c("indicator_code"),
      names_from = "disagg",
      values_from = "msg"
    )

  # unique mismatched data frame
  f_mismatched_u = left_join(
    mismatched,
    disagg_msgs_w
  )

  # full mismatched data frame
  f_mismatched_a =
    left_join(
      model_schema_joined_e %>% filter(age_match == FALSE | sex_match == FALSE | kp_match == FALSE),
      disagg_msgs_w
    )

  # raw full data
  f_all =
    left_join(
      model_schema_joined_e,
      disagg_msgs_w
    )

  # report out list
  list(
    "mismatched_report_uniques" = f_mismatched_u,
    "mismatched_report_full" = f_mismatched_a,
    "raw_report" = f_all
  )

}
