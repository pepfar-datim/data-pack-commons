context("Read a config file")

test_that("We can read a config file", {
    config <- LoadConfigFile(test_config("test-config.json"))
    expect_type(config,"list")
    expect_named(config,c("dhis"))
    expect_named(config$dhis,c("baseurl","username","password"))
    expect_equal(getOption("baseurl"),config$dhis$baseurl)
    expect_equal(getOption("config"),test_config("test-config.json"))
    })



with_mock_api({
  test_that("We can login", {
    test_result <- DHISLogin(test_config("test-config.json"))
    expect_true(test_result)
    expect_equal(getOption("organisationUnit"),"ImspTQPwCqd")

  })
})