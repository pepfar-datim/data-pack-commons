#require(httptest)
# httptest::.mockPaths("/Users/sam/Documents/GitHub/data-pack-commons/tests/testthat")
# datapackcommons::DHISLogin("/users/sam/.secrets/play.json")
# httptest::start_capturing()
# httr::GET(paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
#                                             "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
#                                             "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
# httr::GET(paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
#                                            "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
#                                            "&filter=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&filter=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
# httr::GET("https://play.dhis2.org/NONSENSE")
# httptest::stop_capturing()

context("Test interactions with DHIS 2 analytics")
require(httptest)
#DHISLogin("/users/sam/.secrets/play.json")
------------
test_that("We can get data with GetData_Analytics", {
#httptest::use_mock_api()
  datapackcommons::DHISLogin_Play("2.30")
  dimensions <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
                                "filter", "vihpFUg2WTy", "dx", #PMTCT positive test rate indicator
                                "dimension", "ImspTQPwCqd", "ou", # sierra leone
                                "dimension", "LEVEL-2", "ou", 
                                "filter", "LAST_YEAR", "pe",
                                "dimension", "UOqJW6HPvvL", "veGzholzPQm",
                                "dimension", "WAl0OCcIYxr", "veGzholzPQm",
                                "dimension", "uYxK4wmcPqA", "J5jldMd8OHv",
                                "dimension", "EYbopBOJWsW", "J5jldMd8OHv")
  # veGzholzPQm = HIV age, UOqJW6HPvvL = 15-24y, WAl0OCcIYxr = 25-49y, 
  # J5jldMd8OHv = Facility Type, uYxK4wmcPqA = CHP, EYbopBOJWsW = MCHP
  
  response <- GetData_Analytics(dimensions, base_url = "https://play.dhis2.org/2.30/")
  testthat::expect_equal(response$api_call, paste0("https://play.dhis2.org/2.30/api/29/analytics.json?",
         "dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=ou:ImspTQPwCqd;LEVEL-2",
         "&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr&filter=dx:vihpFUg2WTy",
         "&filter=pe:LAST_YEAR&outputIdScheme=UID&hierarchyMeta=true"))
  testthat::expect_gt(NROW(response$results),0)
  testthat::expect_named(response$results,
                         c("Facility Type", "Organisation unit", 
                           "HIV age", "Value","ou_hierarchy"))
#  httptest:::stop_mocking()
})

test_that("RetryAPI", {
  httptest::use_mock_api()
  api_url = paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
  "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
  "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr")
  testthat::expect_type(RetryAPI(api_url, "application/csv", 1), "list")
  testthat::expect_error(RetryAPI(api_url, "application/json", 1))
  api_url <- "https://play.dhis2.org/NONSENSE"
  testthat::expect_error(RetryAPI(api_url, "text/html", 1))
  httptest::stop_mocking()
})

test_that("ntryLevels", {
  httptest::use_mock_api()
#  DHISLogin("/users/sam/.secrets/prod.json")
  data <- GetCountryLevels(base_url = "https://www.datim.org/")
  expect_gt(NROW(data), 0)
  expect_named(data, c("country_level", "planning_level", "prioritization_level",
                       "facility_level", "community_level",
                       "country_name", "id"))
  
  expect_error(GetCountryLevels(base_url = "https://www.datim.org/", c("nonsense", "Rwanda")))
  expect_error(GetCountryLevels(base_url = "https://www.datim.org/", c("Rwanda", "Rwanda")))
  
  data <- GetCountryLevels(base_url = "https://www.datim.org/", c("Kenya", "Rwanda"))
  expect_equal(NROW(data), 2)
  expect_setequal(data$country_name, c("Kenya", "Rwanda"))
  httptest::stop_mocking()
  })

test_that("ValidateCodeIdPairs", {
  httptest::use_mock_api()
  testthat::expect_true(datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                                             c("IN_52486","IN_52491"), 
                                                             c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                                             "indicators"))
  testthat::expect_true(datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                                             c("IN_52486", "IN_52491", "IN_52491"), 
                                                             c("Uvn6LCg7dVU", "OdiHJayrsKo", "OdiHJayrsKo"), 
                                                             "indicators"))
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                         c("IN_52486","NONSENSE"), 
                                         c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                         "indicators"))
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                         c("IN_52486","IN_52491"), 
                                         c("NONSENSE","OdiHJayrsKo"), 
                                         "indicators"))
  
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                         c("IN_52486","IN_52491"), 
                                         c("Uvn6LCg7dVU"), 
                                         "indicators"))
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs("play.dhis2.org/2.29/", 
                                         c("IN_52486"), 
                                         c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                         "indicators"))
  httptest::stop_mocking()
  })

test_that("ValidateNameIdPairs", {
  httptest::use_mock_api()
testthat::expect_true(datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage","ANC 2 Coverage"), 
                                                           c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                                           "indicators",
                                                           base_url = "play.dhis2.org/2.29/"))
testthat::expect_true(datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "ANC 2 Coverage", "ANC 2 Coverage"), 
                                                           c("Uvn6LCg7dVU", "OdiHJayrsKo", "OdiHJayrsKo"), 
                                                           "indicators",
                                                           base_url = "play.dhis2.org/2.29/"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage","NONSENSE"), 
                                       c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                       "indicators",
                                       base_url = "play.dhis2.org/2.29/"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage","ANC 2 Coverage"), 
                                       c("NONSENSE","OdiHJayrsKo"), 
                                       "indicators",
                                       base_url = "play.dhis2.org/2.29/"))

testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage","ANC 2 Coverage"), 
                                       c("Uvn6LCg7dVU"), 
                                       "indicators",
                                       base_url = "play.dhis2.org/2.29/"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage"), 
                                       c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                       "indicators",
                                       base_url = "play.dhis2.org/2.29/"))

datapackcommons::ValidateNameIdPairs(c("PNC 1","PNC 2"), 
                                     c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                     "indicators", exact = FALSE,
                                     base_url = "https://play.dhis2.org/2.29/") %>% 
  NROW() %>% 
  testthat::expect_equal(2)


testthat::expect_true(datapackcommons::ValidateNameIdPairs(c("ANC 1","Coverage"), 
                                                           c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                                           "indicators", exact = FALSE,
                                                           base_url = "https://play.dhis2.org/2.29/"))
httptest::stop_mocking()
})

# This method is not yet exported
testthat::test_that("GetSqlView", {
  datapackcommons::DHISLogin_Play("2.29")
  
  result <- GetSqlView("qMYMT0iUGkG", "valueType", "TEXT", base_url = "https://play.dhis2.org/2.29/") %>% 
    dplyr::select(valuetype) %>% 
    dplyr::distinct()
  
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result[[1]], "TEXT")
  testthat::expect_error(
    GetSqlView("qMYMT0iUGkG", "valueType", base_url = "https://play.dhis2.org/2.29/") 
    )
  
  result <- GetSqlView("GCZ01m3pIRd",  base_url = "https://play.dhis2.org/2.29/")
  testthat::expect_gt(NROW(result), 0)
  }
  )