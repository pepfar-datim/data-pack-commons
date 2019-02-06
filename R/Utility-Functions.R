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
