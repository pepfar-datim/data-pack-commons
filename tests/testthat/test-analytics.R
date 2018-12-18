context("Test interactions with DHIS 2 analytics")

DHISLogin("/users/sam/.secrets/play.json")

test_that("We can get data with an indicator", {
  response <- GetDataWithIndicator("vihpFUg2WTy", "ImspTQPwCqd", "2",
                                   "LAST_YEAR", data.frame(c("UOqJW6HPvvL","WAl0OCcIYxr","uYxK4wmcPqA","EYbopBOJWsW"),
                                                           c("veGzholzPQm","veGzholzPQm","J5jldMd8OHv","J5jldMd8OHv")))
  expect_equal(response$api_call,paste0("play.dhis2.org/2.29/api/29/analytics.csv?outputIdScheme=UID",
                                        "&dimension=dx:vihpFUg2WTy&dimension=pe:LAST_YEAR&dimension=ou:LEVEL-2;ImspTQPwCqd",
                                        "&dimension=J5jldMd8OHv:uYxK4wmcPqA;EYbopBOJWsW&dimension=veGzholzPQm:UOqJW6HPvvL;WAl0OCcIYxr"))
  expect_is(response$time,"POSIXct")
})
