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
#DHISLogin("/users/sam/.secrets/play.json")
------------
test_that("We can get data with GetData_Analytics", {
#httptest::use_mock_api()
  datapackcommons::DHISLogin_Play("2.29")
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
  
  response <- GetData_Analytics(dimensions, base_url = "https://play.dhis2.org/2.29/")
  testthat::expect_equal(response$api_call, paste0("https://play.dhis2.org/2.29/api/29/analytics.json?",
         "dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=ou:ImspTQPwCqd;LEVEL-2",
         "&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr&filter=dx:vihpFUg2WTy",
         "&filter=pe:LAST_YEAR&outputIdScheme=UID&hierarchyMeta=true"))
  testthat::expect_gt(NROW(response$results),0)
  testthat::expect_named(response$results,
                         c("Facility Type", "Organisation unit", 
                           "HIV age", "Value","ou_hierarchy"))
#  httptest:::stop_mocking()
})


test_that("We can get data with GetDataWithIndicator", {
  httptest::use_mock_api()
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
  expect_equal(response$api_call, paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
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
  httptest::stop_mocking()
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

test_that("GetCountryLevels", {
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
httptest::stop_mocking()
})

# This method is not yet exported
test_that("TransformAnalyticsOutput_SiteTool", {
  # Add sample data at top of the file so its reusable
  # OVC Serv sample data, aggregates two ages together, combine 18-24 and 25+ ages, look at dataset from mapdimtooptions test
  dimensions_sample <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
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
  datapackcommons::DHISLogin_Play("2.29")
  analytics_data <- datapackcommons::GetData_Analytics(dimensions_sample, "https://play.dhis2.org/2.29/")$results
  # Sample analytics_data
  # structure(list(`Facility Type` = c("uYxK4wmcPqA", "uYxK4wmcPqA", 
  #                                    "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", 
  #                                    "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", 
  #                                    "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", "uYxK4wmcPqA", 
  #                                    "uYxK4wmcPqA", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", 
  #                                    "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", 
  #                                    "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", 
  #                                    "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW", "EYbopBOJWsW"), 
  #                `Organisation unit` = c("fdc6uOvgoji", "fdc6uOvgoji", "lc3eMKXaEfw", 
  #                                        "lc3eMKXaEfw", "jUb8gELQApl", "jUb8gELQApl", "kJq2mPyFEHo", 
  #                                        "kJq2mPyFEHo", "qhqAxPSTUXp", "qhqAxPSTUXp", "Vth0fbpFcsO", 
  #                                        "Vth0fbpFcsO", "jmIPBj66vD6", "jmIPBj66vD6", "bL4ooGhyHRQ", 
  #                                        "bL4ooGhyHRQ", "at6UHUQatSo", "at6UHUQatSo", "fdc6uOvgoji", 
  #                                        "fdc6uOvgoji", "lc3eMKXaEfw", "lc3eMKXaEfw", "PMa2VCrupOd", 
  #                                        "PMa2VCrupOd", "kJq2mPyFEHo", "kJq2mPyFEHo", "qhqAxPSTUXp", 
  #                                        "qhqAxPSTUXp", "Vth0fbpFcsO", "Vth0fbpFcsO", "jmIPBj66vD6", 
  #                                        "jmIPBj66vD6", "bL4ooGhyHRQ", "bL4ooGhyHRQ", "at6UHUQatSo", 
  #                                        "at6UHUQatSo"), `HIV age` = c("UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr", "UOqJW6HPvvL", "WAl0OCcIYxr", 
  #                                                                      "UOqJW6HPvvL", "WAl0OCcIYxr"), Value = c(0, 0, 0.96, 0.44, 
  #                                                                                                               0, 0, 1.7, 1.1, 0, 0, 4.8, 3.9, 0, 0, 0.66, 0, 15.5, 9.5, 
  #                                                                                                               0, 1.7, 4.8, 5.7, 0, 0, 1, 2.1, 0.6, 0.85, 3, 2.6, 3.4, 0, 
  #                                                                                                               0, 0, 16.2, 15.9), ou_hierarchy = list(c("ImspTQPwCqd", "fdc6uOvgoji"
  #                                                                                                               ), c("ImspTQPwCqd", "fdc6uOvgoji"), c("ImspTQPwCqd", "lc3eMKXaEfw"
  #                                                                                                               ), c("ImspTQPwCqd", "lc3eMKXaEfw"), c("ImspTQPwCqd", "jUb8gELQApl"
  #                                                                                                               ), c("ImspTQPwCqd", "jUb8gELQApl"), c("ImspTQPwCqd", "kJq2mPyFEHo"
  #                                                                                                               ), c("ImspTQPwCqd", "kJq2mPyFEHo"), c("ImspTQPwCqd", "qhqAxPSTUXp"
  #                                                                                                               ), c("ImspTQPwCqd", "qhqAxPSTUXp"), c("ImspTQPwCqd", "Vth0fbpFcsO"
  #                                                                                                               ), c("ImspTQPwCqd", "Vth0fbpFcsO"), c("ImspTQPwCqd", "jmIPBj66vD6"
  #                                                                                                               ), c("ImspTQPwCqd", "jmIPBj66vD6"), c("ImspTQPwCqd", "bL4ooGhyHRQ"
  #                                                                                                               ), c("ImspTQPwCqd", "bL4ooGhyHRQ"), c("ImspTQPwCqd", "at6UHUQatSo"
  #                                                                                                               ), c("ImspTQPwCqd", "at6UHUQatSo"), c("ImspTQPwCqd", "fdc6uOvgoji"
  #                                                                                                               ), c("ImspTQPwCqd", "fdc6uOvgoji"), c("ImspTQPwCqd", "lc3eMKXaEfw"
  #                                                                                                               ), c("ImspTQPwCqd", "lc3eMKXaEfw"), c("ImspTQPwCqd", "PMa2VCrupOd"
  #                                                                                                               ), c("ImspTQPwCqd", "PMa2VCrupOd"), c("ImspTQPwCqd", "kJq2mPyFEHo"
  #                                                                                                               ), c("ImspTQPwCqd", "kJq2mPyFEHo"), c("ImspTQPwCqd", "qhqAxPSTUXp"
  #                                                                                                               ), c("ImspTQPwCqd", "qhqAxPSTUXp"), c("ImspTQPwCqd", "Vth0fbpFcsO"
  #                                                                                                               ), c("ImspTQPwCqd", "Vth0fbpFcsO"), c("ImspTQPwCqd", "jmIPBj66vD6"
  #                                                                                                               ), c("ImspTQPwCqd", "jmIPBj66vD6"), c("ImspTQPwCqd", "bL4ooGhyHRQ"
  #                                                                                                               ), c("ImspTQPwCqd", "bL4ooGhyHRQ"), c("ImspTQPwCqd", "at6UHUQatSo"
  #                                                                                                               ), c("ImspTQPwCqd", "at6UHUQatSo"))), class = c("tbl_df", 
  #                                                                                                                                                               "tbl", "data.frame"), row.names = c(NA, -36L))
  data_element_map = datapackcommons::Map19Tto20T[36,]
  # Sample data_element_map
  # structure(list(indicatorCode_fy20_cop = "TB_ART.N.Age/Sex/NewExistingART/HIVStatus.20T.Already", 
  #                `Technical Area_fy20_cop` = "TB_ART", `Numerator / Denominator_fy20_cop` = "N", 
  #                `Disagregation Type_fy20_cop` = "Age/Sex/NewExistingART/HIVStatus", 
  #                Other_fy20_cop = "Already", dx = "DE_GROUP-zhdJiWlPvCz", 
  #                technical_area = "TB_ART", technical_area_uid = "Z6TU9Os82Yw", 
  #                num_or_den = "Numerator", num_or_den_uid = "Som9NRMQqV7", 
  #                disagg_type = "Age Aggregated/Sex/HIVStatus", disagg_type_uid = "h0pvSVe1TYf", 
  #                pe = "2018Oct", age_set = "<15/>15.d", sex_set = "F/M", kp_set = NA_character_, 
  #                other_disagg = NA_character_, allocate = "distribute"), class = c("tbl_df", 
  #                                                                                  "tbl", "data.frame"), row.names = c(NA, -1L))
  
  analytics_data <- structure(list(`Age: Cascade Age bands` = c("noEF3Mx7aBu", "Z8MTaDxRBP6", 
                                                                "noEF3Mx7aBu"), `Disaggregation Type` = c("Qbz6SrpmJ1y", "Qbz6SrpmJ1y", 
                                                                                                          "Qbz6SrpmJ1y"), `Cascade sex` = c("hDBPKTjUPDm", "Gxcf2DK8vNc", 
                                                                                                                                            "ZOYVg7Hosni"), `Numerator / Denominator` = c("Som9NRMQqV7", 
                                                                                                                                                                                          "Som9NRMQqV7", "Som9NRMQqV7"), `Technical Area` = c("RxyNwEV3oQf", 
                                                                                                                                                                                                                                              "RxyNwEV3oQf", "RxyNwEV3oQf"), `Organisation unit` = c("qYzGABaWyCf", 
                                                                                                                                                                                                                                                                                                     "Z3IDOaDDkwG", "nxGb6sd7p7D"), `Funding Mechanism` = c("eX9p46QTgjL", 
                                                                                                                                                                                                                                                                                                                                                            "Y8JtX87RYYP", "eX9p46QTgjL"), Value = c(108, 807, 120), ou_hierarchy = list(
                                                                                                                                                                                                                                                                                                                                                              c("ybg3MO3hcf4", "KSkooYTy8FB", "V0qMZH29CtN", "qYzGABaWyCf"
                                                                                                                                                                                                                                                                                                                                                              ), c("ybg3MO3hcf4", "KSkooYTy8FB", "V0qMZH29CtN", "Z3IDOaDDkwG"
                                                                                                                                                                                                                                                                                                                                                              ), c("ybg3MO3hcf4", "KSkooYTy8FB", "V0qMZH29CtN", "nxGb6sd7p7D"
                                                                                                                                                                                                                                                                                                                                                              ))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                           -3L))
  
  # MapDimToOptions not found (so prefaced it in SiteDistMain)
  test_output <- TransformAnalyticsOutput_SiteTool(analytics_data, datapackcommons::dim_item_sets,
                                                                    data_element_map, 4)
})
