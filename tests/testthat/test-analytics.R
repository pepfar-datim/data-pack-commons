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

test_that("GetCountryLevels", {
#  DHISLogin("/users/sam/.secrets/prod.json")
  data <- GetCountryLevels(base_url = "https://www.datim.org/")
  expect_gt(NROW(data), 0)
  expect_named(data, c("country_level", "planning_level", "prioritization_level",
                       "country_name", "id"))
  
  expect_error(GetCountryLevels(base_url = "https://www.datim.org/", c("nonsense", "Rwanda")))
  expect_error(GetCountryLevels(base_url = "https://www.datim.org/", c("Rwanda", "Rwanda")))
  
  data <- GetCountryLevels(base_url = "https://www.datim.org/", c("Kenya", "Rwanda"))
  expect_equal(NROW(data), 2)
  expect_setequal(data$country_name, c("Kenya", "Rwanda"))
})


httptest::stop_mocking()