# require(httptest)
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
httptest::use_mock_api()
#DHISLogin("/users/sam/.secrets/play.json")
test_that("We can get data with GetDataWithIndicator", {
  item_to_dimension <- data.frame(c("UOqJW6HPvvL", "WAl0OCcIYxr", "uYxK4wmcPqA", "EYbopBOJWsW"),
                            c("veGzholzPQm", "veGzholzPQm", "J5jldMd8OHv", "J5jldMd8OHv"))

# veGzholzPQm = HIV age, UOqJW6HPvvL = 15-24y, WAl0OCcIYxr = 25-49y, 
# J5jldMd8OHv = Facility Type, uYxK4wmcPqA = CHP, EYbopBOJWsW = MCHP
    
  response <- GetDataWithIndicator(base_url = "https://play.dhis2.org/2.29/", indicator = "vihpFUg2WTy",   #PMTCT positive test rate indicator
                                   org_units = "ImspTQPwCqd",   # Sierra Leone ou
                                   level = "2",             # level 2 - districst
                                   "LAST_YEAR",     # last calendar year
                                   additional_dimensions = item_to_dimension)     # 2 column data fram of items and dimension
# API call constructed as we expect
  expect_equal(response$api_call,paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
                                        "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
                                        "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
# Get back a time the call was made
  expect_is(response$time,"POSIXct")
  
# Respose has one or more rows
  expect_gt(NROW(response), 0)
  
# Respose contains expected columns
  expect_named(response$results, c("Data", "Period", "Organisation unit", "Facility Type", "HIV age", "Value"))
  
# now do filter instead of dimension
  response_filter <-  GetDataWithIndicator(base_url = "https://play.dhis2.org/2.29/", indicator = "vihpFUg2WTy",   #PMTCT positive test rate indicator
                                           org_units = "ImspTQPwCqd",   # Sierra Leone ou
                                           level = "2",             # level 2 - districst
                                           "LAST_YEAR",     # last calendar year
                                           additional_filters = item_to_dimension)     # 2 column data fram of items and dimension

# API call constructed as we expect
  expect_equal(response_filter$api_call,paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
                                        "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
                                        "&filter=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&filter=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
  
# Sum of values from response and response_filter should be the same
  expect_equal(sum(response$Value), sum(response_filter$Value))
  })

test_that("RetryAPI", {
  api_url = paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
  "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
  "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr")
  testthat::expect_type(RetryAPI(api_url, "application/csv", 1), "list")
  testthat::expect_error(RetryAPI(api_url, "application/json", 1))
  api_url <- "https://play.dhis2.org/NONSENSE"
  testthat::expect_error(RetryAPI(api_url, "text/html", 1))
  
})

test_that("GetCountryLevels", {
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
})

test_that("ValidateCodeIdPairs", {
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
  })

test_that("ValidateNameIdPairs", {
testthat::expect_true(datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                                           c("ANC 1 Coverage","ANC 2 Coverage"), 
                                                           c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                                           "indicators"))
testthat::expect_true(datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                                             c("ANC 1 Coverage", "ANC 2 Coverage", "ANC 2 Coverage"), 
                                                             c("Uvn6LCg7dVU", "OdiHJayrsKo", "OdiHJayrsKo"), 
                                                             "indicators"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                       c("ANC 1 Coverage","NONSENSE"), 
                                       c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                       "indicators"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                       c("ANC 1 Coverage","ANC 2 Coverage"), 
                                       c("NONSENSE","OdiHJayrsKo"), 
                                       "indicators"))

testthat::expect_error(
  datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                       c("ANC 1 Coverage","ANC 2 Coverage"), 
                                       c("Uvn6LCg7dVU"), 
                                       "indicators"))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs("play.dhis2.org/2.29/", 
                                       c("ANC 1 Coverage"), 
                                       c("Uvn6LCg7dVU","OdiHJayrsKo"), 
                                       "indicators"))
})


httptest::stop_mocking()

testthat::test_that("Creating PSNU levels and Org unit data to obtain site list data", {
  PSNU_level_test <-
    data.frame(
      "country_level" = 3,
      "planning_level" = 5,
      "prioritization_level" = 5,
      "facility_level" = 7,
      "community_level" = 6,
      "country_name" = "TestCountry",
      "id" = "TestId"
    )
  org_list <-
    data.frame(
      "organisationunitid" = 1234,
      "organisationunituid" = "UnitId",
      "name" = "NameOfTheSite",
      "level" = 6,
      "uidlevel1" = "Level1Uid",
      "level1name" = "NameAtLevel1",
      "uidlevel2" = "Level2Uid",
      "level2name" = "NameAtLevel2",
      "uidlevel3" = "TestId",
      "level3name" = "TestCountry",
      "uidlevel4" = "Level4Name",
      "level4name" = "NameAtLevel4",
      "uidlevel5" = "Level5Name",
      "level5name" = "NameAtLevel5",
      "uidlevel6" = "UnitId",
      "level6name" = "NameOfTheSite",
      "uidlevel7" = NA,
      "level7name" = NA,
      "uidlevel8" = NA,
      "level8name" = NA,
      "uidlevel9" = NA,
      "level9name" = NA
    )
})