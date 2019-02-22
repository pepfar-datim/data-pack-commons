#' @export
#' @title StackPrefixedCols(data, prefixes)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
StackPrefixedCols <- function(data, prefixes){
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
FormatForApi_Dimensions <- function(data, type_col, dim_id_col, item_id_col){
  assertthat::assert_that(assertthat::has_name(data, type_col),
                          assertthat::has_name(data, dim_id_col),
                          assertthat::has_name(data, item_id_col))
  data %>% dplyr::mutate(type = data[[type_col]],
                         dim_id = data[[dim_id_col]],
                         item_id = data[[item_id_col]])  %>%
    dplyr::select(type, dim_id, item_id) %>% unique() %>% 
    dplyr::group_by_at(c("type", "dim_id"))  %>%  
    dplyr::summarise(items = paste0(item_id, collapse = ";")) %>% 
     dplyr::ungroup() %>% 
     dplyr::transmute(component = glue::glue("{type}={dim_id}:{items}")) %>% 
     .[[1]] %>% 
     paste0(collapse="&")
}

#' @export
#' @title RenameDimensionColumns(data, type)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
RenameDimensionColumns <- function(data, type){
  data %>% dplyr::rename(!!paste0(type,"_dim_uid") := dim_uid,
                         !!paste0(type,"_dim_name") := dim_name,
                         !!paste0(type,"_dim_cop_type") := dim_cop_type,
                         !!paste0(type,"_dim_item_name") := dim_item_name,
                         !!paste0(type,"_option_name") := option_name,
                         !!paste0(type,"_option_uid") := option_uid,
                         !!paste0(type,"_sort_order") := sort_order,
                         !!paste0(type,"_weight") := weight,
                         !!paste0(type,"_model_sets") := model_sets) %>% return()
}

#' @export
#' @title MapDimToOptions(data, items_to_options, allocate)
#' 
#' @description Takes columns from data with specified prefixes and stacks them based on the unprefixed
#' portion of the name. Columns not containing one of the prefixes are excluded in returned data. 
#' @param data dataframe - contains data to stack 
#' @param prefixes string vector - list of prefixes to include in column selection and stacking
#' @return  tibble with superset of columns without prefixes in column names
#'
MapDimToOptions <- function(data, items_to_options, allocate){
  
  if(NROW(items_to_options) == 0){
    return(data)
  }
  
  dimension_uid <- unique(items_to_options$dim_uid)
  cop_category <- unique(items_to_options$dim_cop_type)
  assertthat::assert_that(NROW(dimension_uid) == 1, NROW(cop_category) == 1)
  
  if(is.na(dimension_uid)){
    # We are in a scenario of distributing to category options in the absence of a source dimension
    # so we need cartesian product of data with item_to_dim entries
    joined_data <- tidyr::crossing(data, items_to_options)
  } else {
    dim_name <-  items_to_options[[1,"dim_name"]]
    joined_data <- data %>%
      left_join(items_to_options, by = setNames("dim_item_uid", dim_name))
  }
  
  if(allocate == "distribute"){
    joined_data %>%
      mutate(Value = Value * weight) %>%
      RenameDimensionColumns(stringr::str_remove(cop_category, "_set"))
  } else{
    joined_data %>%
      RenameDimensionColumns(stringr::str_remove(cop_category, "_set"))
  }
}

