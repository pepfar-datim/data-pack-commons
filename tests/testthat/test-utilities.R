context("Test helper utility functions")

test_that("StackPrefixedCols", {
  data  <-  data.frame(A.x = c(1,1), 
                    B.x = c(2,2), 
                    A.A.x=c(3,3), 
                    B.z=c(100,100), 
                    b.extra=c(100,100))
  prefixes = c("A.", "B.")
  result <- StackPrefixedCols(data, prefixes)
  testthat::expect_named(result, c("x", "A.x", "z"))
    # A.A.x doesn't become just x
    # b.extra gets removed
  sum(result$x) %>% testthat::expect_equal(6)
  NROW(result) %>% testthat::expect_equal(4)
    # A.x and B.x columns got stacked
  sum(is.na(result$z)) %>% 
    testthat::expect_equal(2)
    # The z column in B. but not A. gets included with NAs for the A. rows
  testthat::expect_error(StackPrefixedCols(data, prefixes=NULL))
  testthat::expect_error(StackPrefixedCols(c(1,2), prefixes))
  testthat::expect_error(StackPrefixedCols(NULL, prefixes))

  })

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
  testthat::expect_true(datapackcommons::all_sites_list(PSNU_level_test,org_list)$Site_Type == "community")
  
  org_list$level <- 7
  testthat::expect_true(datapackcommons::all_sites_list(PSNU_level_test,org_list)$Site_Type == "facility")
  
  org_list$level <- 5
  testthat::expect_true(datapackcommons::all_sites_list(PSNU_level_test,org_list)$Site_Type == "psnu")
  
  
})
