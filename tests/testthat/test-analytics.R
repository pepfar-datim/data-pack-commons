#require(httptest)
# httptest::.mockPaths("/Users/sam/Documents/GitHub/data-pack-commons/tests/testthat")
# datapackcommons::DHISLogin("/users/sam/.secrets/play.json")
# httptest::start_capturing()
# httr::GET(paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
#                  "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
#                  "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
# httr::GET(paste0("https://play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
#                  "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
#                  "&filter=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&filter=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
# httr::GET("https://play.dhis2.org/NONSENSE")
# httptest::stop_capturing()

context("Test interactions with DHIS 2 analytics")
require(httptest)
#DHISLogin("/users/sam/.secrets/play.json")

httptest::use_mock_api()
test_that("ValidateCodeIdPairs", {

  play_session_mock <- list(base_url = "https://play.dhis2.org/2.29/",
                            handle = httr::handle("https://play.dhis2.org/2.29/"))
  testthat::expect_true(datapackcommons::ValidateCodeIdPairs(c("IN_52486", "IN_52491"),
                                                             c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                                             "indicators",
                                                             d2_session = play_session_mock))
  testthat::expect_true(datapackcommons::ValidateCodeIdPairs(c("IN_52486", "IN_52491", "IN_52491"),
                                                             c("Uvn6LCg7dVU", "OdiHJayrsKo", "OdiHJayrsKo"),
                                                             "indicators",
                                                             d2_session = play_session_mock))
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs(c("IN_52486", "NONSENSE"),
                                         c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                         "indicators"),
    d2_session = play_session_mock)
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs(c("IN_52486", "IN_52491"),
                                         c("NONSENSE", "OdiHJayrsKo"),
                                         "indicators"),
    d2_session = play_session_mock)

  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs(c("IN_52486", "IN_52491"),
                                         c("Uvn6LCg7dVU"),
                                         "indicators"),
    d2_session = play_session_mock)
  testthat::expect_error(
    datapackcommons::ValidateCodeIdPairs(c("IN_52486"),
                                         c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                         "indicators"),
    d2_session = play_session_mock)

  })
httptest::stop_mocking()

httptest::use_mock_api()
test_that("ValidateNameIdPairs", {
  play_session_mock <- list(base_url = "https://play.dhis2.org/2.29/",
                            handle = httr::handle("https://play.dhis2.org/2.29/"))
testthat::expect_true(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "ANC 2 Coverage"),
                                       c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                       "indicators",
                                       d2_session = play_session_mock))
  testthat::expect_true(datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "ANC 2 Coverage", "ANC 2 Coverage"),
                                                             c("Uvn6LCg7dVU", "OdiHJayrsKo", "OdiHJayrsKo"),
                                                             "indicators",
                                                             d2_session = play_session_mock))
  testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "NONSENSE"),
                                       c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                       "indicators",
                                       d2_session = play_session_mock))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "ANC 2 Coverage"),
                                       c("NONSENSE", "OdiHJayrsKo"),
                                       "indicators",
                                       d2_session = play_session_mock))

testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage", "ANC 2 Coverage"),
                                       c("Uvn6LCg7dVU"),
                                       "indicators",
                                       d2_session = play_session_mock))
testthat::expect_error(
  datapackcommons::ValidateNameIdPairs(c("ANC 1 Coverage"),
                                       c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                       "indicators",
                                       d2_session = play_session_mock))

datapackcommons::ValidateNameIdPairs(c("PNC 1", "PNC 2"),
                                     c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                     "indicators", exact = FALSE,
                                     d2_session = play_session_mock) %>%
  NROW() %>%
  testthat::expect_equal(2)


testthat::expect_true(datapackcommons::ValidateNameIdPairs(c("ANC 1", "Coverage"),
                                                           c("Uvn6LCg7dVU", "OdiHJayrsKo"),
                                                           "indicators", exact = FALSE,
                                                           d2_session = play_session_mock))
})
httptest::stop_mocking()


