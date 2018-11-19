#' \code{datapackcommons} package
#'
#' Parsing and import functions for the ICPI Data Pack
#' https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
#' @docType package
#' @name datapackcommons
#' @importFrom dplyr %>%
#' @importFrom utils URLencode
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c( ".",
       "web_api_call"
    )
  )