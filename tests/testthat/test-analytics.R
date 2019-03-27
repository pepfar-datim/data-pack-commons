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
testthat::test_that("TransformAnalyticsOutput_SiteTool", {
  
  # Data element map row for OVC SERV
  data_element_map = structure(list(indicatorCode_fy20_cop = "OVC_SERV.N.Age/Sex/ProgramStatus.20T.Active",
                                   `Technical Area_fy20_cop` = "OVC_SERV", `Numerator / Denominator_fy20_cop` = "N",
                                   `Disagregation Type_fy20_cop` = "Age/Sex/ProgramStatus",
                                   Other_fy20_cop = "Active", dx = "DE_GROUP-zhdJiWlPvCz", technical_area = "OVC_SERV",
                                   technical_area_uid = "RxyNwEV3oQf", num_or_den = "Numerator",
                                   num_or_den_uid = "Som9NRMQqV7", disagg_type = "Age/Sex",
                                   disagg_type_uid = "Qbz6SrpmJ1y", pe = "2018Oct", age_set = "<1-18+",
                                   sex_set = "F/M/U", kp_set = NA_character_, other_disagg = NA_character_,
                                   allocate = "distribute"), class = c("tbl_df", "tbl", "data.frame"
                                   ), row.names = c(NA, -1L))
                                                                                                                                                                                                                                                                                                                                                                                                                    
  sample_data_1to9 <- structure(list(`Age: Cascade Age bands` = "egW0hBcZeD2", `Disaggregation Type` = "Qbz6SrpmJ1y", 
                                     `Cascade sex` = "Gxcf2DK8vNc", `Numerator / Denominator` = "Som9NRMQqV7", 
                                     `Technical Area` = "RxyNwEV3oQf", `Organisation unit` = "Test1", 
                                     `Funding Mechanism` = "Test2", Value = 333, ou_hierarchy = list(
                                       c("Test3", "Test4", "Test5", "Test1"
                                       ))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  
  sample_data_18to24 <- structure(list(`Age: Cascade Age bands` = "N0PwGN3UKWx", `Disaggregation Type` = "Qbz6SrpmJ1y", 
                                       `Cascade sex` = "hDBPKTjUPDm", `Numerator / Denominator` = "Som9NRMQqV7", 
                                       `Technical Area` = "RxyNwEV3oQf", `Organisation unit` = "Test1", 
                                       `Funding Mechanism` = "Test2", Value = 333, ou_hierarchy = list(
                                         c("Test3", "Test4", "Test5", "Test1"
                                         ))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  
  sample_data_25_plus <- structure(list(`Age: Cascade Age bands` = "pk98FEsOJcz", `Disaggregation Type` = "Qbz6SrpmJ1y", 
                                        `Cascade sex` = "hDBPKTjUPDm", `Numerator / Denominator` = "Som9NRMQqV7", 
                                        `Technical Area` = "RxyNwEV3oQf", `Organisation unit` = "Test1", 
                                        `Funding Mechanism` = "Test2", Value = 333, ou_hierarchy = list(
                                          c("Test3", "Test4", "Test5", "Test1"
                                          ))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
  
  # MapDimToOptions not found (so prefaced it in SiteDistMain)
  test_output <- TransformAnalyticsOutput_SiteTool(sample_data_1to9, datapackcommons::dim_item_sets,
                                                                    data_element_map, 4)
  
  disagg_input <- rbind(sample_data_1to9, sample_data_18to24, sample_data_25_plus)
  # I can dput the binded rows but creating them separately is more explanatory.
  
  agg_output <- TransformAnalyticsOutput_SiteTool(disagg_input, datapackcommons::dim_item_sets,
                                                  data_element_map, 4)
  
  # Test to check that the value is being aggregated
  testthat::expect_equal(agg_output[["aggregations"]]$Value, disagg_input$Value[2]*2)
  
  # Test to check that the right age option is being aggregated
  testthat::expect_equal("Q6xWcyHDq6e", agg_output[["aggregations"]]$age_option_uid)
  
  # Asserts that all other rows have disagg values
  non_agg_data <- agg_output[["processed"]] %>% dplyr::filter(age_option_uid != "Q6xWcyHDq6e")
  testthat::expect_equal(non_agg_data$Value, rep(333/nrow(non_agg_data), times = (4)))
  
  # Making sure the age disaggs happen for input ages 1-9
  testthat::expect_equal(non_agg_data$age_option_name, rep(c("5-9","1-4"), times = (2)))
  
  # Test the ou hierarchy is dropped and psnu level id is pulled out
  # Using grep for the test as it sends the location of a string and hence can help detect ou hierarchy in any object
  # and returns "integer(0)" if an element is not present
  # ou_hierarchy is as follows: c("Test3", "Test4", "Test5", "Test1"), with Test1 being the PSNU level UID
  testthat::expect_equal(grep("Test3", disagg_input), 9)
  testthat::expect_equal(grep("Test3", agg_output), integer(0))
  
  testthat::expect_equal(grep("Test4", disagg_input), 9)
  testthat::expect_equal(grep("Test4", agg_output), integer(0))
  
  testthat::expect_equal(grep("Test5", disagg_input), 9)
  testthat::expect_equal(grep("Test5", agg_output), integer(0))
  
  testthat::expect_equal(grep("Test1", disagg_input), c(6, 9))
  testthat::expect_equal(grep("Test1", agg_output), c(1, 3))
})
