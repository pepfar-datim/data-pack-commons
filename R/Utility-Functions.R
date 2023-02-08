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
#' @param ancestors_data list - optional ancestor data, mainly for testing
#' @param data_psnu character vector of psnu data, mainly for testing
#' @param full_diff If TRUE compares model difference fully
#' @return deltas representing difference in data of both models
#'
diffSnuximModels <- function(model_old, model_new,
                             data_ancestors = NULL,
                             data_psnu = NULL,
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
  if (!is.null(data_ancestors)) {
    ancestors <- data_ancestors
  } else {
    ancestors <- datimutils::getOrgUnits(deltas$psnu_uid,
                                         fields = "ancestors[name]",
                                         d2_session = d2_session)
  }

  if (!is.null(data_psnu)) {
    deltas <- dplyr::mutate(deltas,
                            psnu = data_psnu,
                            ou = purrr::map_chr(ancestors, purrr::pluck, 1, 3),
                            snu1 = purrr::map_chr(ancestors, purrr::pluck, 1, 4, .default = NA_character_))
  } else {
    deltas <- dplyr::mutate(deltas,
                            psnu = datimutils::getOrgUnits(psnu_uid,
                                                           d2_session = d2_session),
                            ou = purrr::map_chr(ancestors, purrr::pluck, 1, 3),
                            snu1 = purrr::map_chr(ancestors, purrr::pluck, 1, 4, .default = NA_character_))
  }

  return(deltas)
}

#' @export
#' @title diffDataEntryForm(uid_a, uid_b)
#'
#' @description A function that compares two data entry forms based of uid inputs
#' that access sqlviews
#' @param dataset_a a dataset uid
#' @param dataset_b a dataset uid
#' @sql_view_data_a for testing can provide manual dataframe input
#' @sql_view_data_b for testing can provide manual dataframe input
#' @param d2_session
#' @return a list of all three differences
#'
diffDataEntryForm <- function(
    dataset_a,
    dataset_b,
    sql_view_data_a = NULL,
    sql_view_data_b = NULL,
    d2_session = dynGet("d2_default_session", inherits = TRUE)
) {

  # for testing purposes we add manual data passing
  # stop function run if these params are partially defined
  if ((!is.null(sql_view_data_a) &&
     is.null(sql_view_data_b)) ||
    (!is.null(sql_view_data_b) &&
     is.null(sql_view_data_a))) {
    stop("If atttempting to pass data in manually both params a and b must be defined!")
  }

  # pull the sql views
  # for testing purposes offer the option of passing manual data
  # this helps us understand what the function actually does
  if (is.null(sql_view_data_a)) {
    x <-  datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                             variable_keys = c("dataSets"),
                             variable_values = c(dataset_a)) %>%
        dplyr::rename("A.dataset" = "dataset")
  } else {
    x <- sql_view_data_a
  }

  if (is.null(sql_view_data_b)) {
    y <- datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                variable_keys = c("dataSets"),
                                variable_values = c(dataset_b)) %>%
      dplyr::rename("B.dataset" = "dataset")
  } else {
    y <- sql_view_data_b
  }

  # in x but not y
  x_not_y <- tryCatch({
    dplyr::anti_join(x, y)
  }, error = function(e) {
    print(e)
    stop("in X but not Y could not be calculated because of error ^")
  })

  # in y but not x
  y_not_x <- tryCatch({
    dplyr::anti_join(y, x)
  }, error = function(e) {
    print(e)
    stop("in Y but not X could not be calculated because of error ^")
  })

  # in x and y
  x_and_y <- tryCatch({
    dplyr::inner_join(x, y)
  }, error = function(e) {
    print(e)
    stop("in X and Y, could not be calculated because of error ^")
  })

  # list
  diff_list <-
    list(
      "x_not_y" = x_not_y,
      "y_not_x" = y_not_x,
      "x_and_y" = x_and_y
    )

  return(diff_list)
}
