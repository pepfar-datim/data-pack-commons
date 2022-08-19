context("Read some metadata")

with_mock_api({
test_that("We can load data elements", {
  play_230 <- list(base_url = "https://play.dhis2.org/2.30/",
                   handle = httr::handle("https://play.dhis2.org/2.30/"))
  test_filters <- "name:like:HIV positive"
  test_fields <- c("id", "name", "shortName")
  test_de_payload <- datimutils::getMetadata("dataElements",
                               filters = test_filters,
                               fields = test_fields,
                               d2_session = play_230)

  expect_type(test_de_payload, "list")
  expect_setequal(names(test_de_payload$dataElements), c(test_fields))
}) })

with_mock_api({
  test_that("We produce an error on a bad end point", {
    expect_error(datimutils::getMetadata("foo"))
  }) })
