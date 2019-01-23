context("Read some metadata")

with_mock_api({
test_that("We can load data elements", {
  test_login <- DHISLogin(test_config("test-config.json"))
  test_filters="name:like:HIV positive"
  test_fields=c("id","name","shortName")
  test_de_payload<-getMetadata("https://play.dhis2.org/2.30/", "dataElements",filters=test_filters,fields=test_fields)
  expect_type(test_de_payload,"list")
  expect_setequal(names(test_de_payload),c(test_fields))
}) })

with_mock_api({
  test_that("We produce an error on a bad end point", {
    test_login <- DHISLogin(test_config("test-config.json"))
    expect_error(getMetadata("foo"))
  }) })
