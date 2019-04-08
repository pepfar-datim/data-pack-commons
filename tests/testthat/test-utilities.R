context("Test helper utility functions")

test_that("StackPrefixedCols", {
  data  <-  data.frame(
    A.x = c(1, 1),
    B.x = c(2, 2),
    A.A.x = c(3, 3),
    B.z = c(100, 100),
    b.extra = c(100, 100)
  )
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
  testthat::expect_error(StackPrefixedCols(data, prefixes = NULL))
  testthat::expect_error(StackPrefixedCols(c(1, 2), prefixes))
  testthat::expect_error(StackPrefixedCols(NULL, prefixes))
  
})

test_that("FormatForApi_Dimensions", {
  # https://play.dhis2.org/2.29/api/29/analytics.json?
  # dimension=LFsZ8v5v7rq:CW81uF03hvV;C6nZpLKjEJr
  # &dimension=ou:O6uvpzGd5pu
  # &filter=dx:BOSZApCrBni;dGdeotKpRed;eRwOwCpMzyP;zYkwbCBALhn
  # &filter=pe:THIS_FINANCIAL_YEAR
  df = tibble::tribble(~ type, ~ dim_id, ~ item_id, ~ other_col,
    "dimension", "LFsZ8v5v7rq", "CW81uF03hvV", "Implementing Partner: AIDSRelief Consortium",
    "dimension", "LFsZ8v5v7rq", "C6nZpLKjEJr", "Implementing Partner: African Medical and Research Foundation",
    "filter", "dx", "BOSZApCrBni", "ART enrollment stage 1",
    "filter", "dx", "dGdeotKpRed", "ART enrollment stage 2",
    "dimension", "ou", "O6uvpzGd5pu", "Bo",
    "filter", "pe", "THIS_FINANCIAL_YEAR", ""
    )
  FormatForApi_Dimensions(df, "type", "dim_id", "item_id") %>%
    testthat::expect_equal(
      "dimension=LFsZ8v5v7rq:CW81uF03hvV;C6nZpLKjEJr&dimension=ou:O6uvpzGd5pu&filter=dx:BOSZApCrBni;dGdeotKpRed&filter=pe:THIS_FINANCIAL_YEAR"
    )
  testthat::expect_error(FormatForApi_Dimensions(df, "type", "dim_id", "Nonsense"))
})

test_that("RenameDimensionColumns", {
  df_1 <-  
    tibble::tribble(~ dim_uid, ~ dim_name, ~ dim_cop_type, ~ dim_item_name, ~ option_name,
                    ~ option_uid, ~ sort_order, ~ weight, ~ model_sets,
                    
                    "test_1", "test_2", "test_3", "test_4", "test_5",
                    "test_6", "test_7", "test_8", "test_9"
  )
  colnames(RenameDimensionColumns(df_1, "test")) %>%
    testthat::expect_equal(
      c(
        "test_dim_uid",
        "test_dim_name",
        "test_dim_cop_type",
        "test_dim_item_name",
        "test_option_name",
        "test_option_uid",
        "test_sort_order",
        "test_weight",
        "test_model_sets"
      )
    )
  df_2 = tibble::tribble(
    ~ dim_name,
    ~ dim_name,
    ~ dim_cop_type,
    ~ dim_item_name,
    ~ option_name,
    ~ option_uid,
    ~ sort_order,
    ~ weight,
    ~ model_sets,
    "test_1",
    "test_2",
    "test_3",
    "test_4",
    "test_5",
    "test_6",
    "test_7",
    "test_8",
    "test_9"
  )
  testthat::expect_error(RenameDimensionColumns(df_2, "test"))
})

test_that("MapDimToOptions", {
  value <- 10
  # Sample data for Cascade sex, taking a few rows with value being 10 for each of them
  sample_data <-  tibble::tribble(
    ~ `Cascade sex`, ~ Value,
    "ZOYVg7Hosni", value,
    "Gxcf2DK8vNc", value,
    "hDBPKTjUPDm", value
  )
  
  # Taking the sex dimention item sets with model sets as "F/M/U"
  sex_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "F/M/U")
  
  
  unique(sex_set$dim_name) %>%
    testthat::expect_equal("Cascade sex")
  
  joined_output_1 <-
    datapackcommons::MapDimToOptions(sample_data, sex_set, "distribute")
  joined_output_1$Value %>%
    testthat::expect_equal(joined_output_1$sex_weight * value)
  
  nrow(joined_output_1 %>%
         dplyr::filter((sex_dim_item_name == "Unspecified sex"))) %>%
    testthat::expect_equal(2)
  
  
  missing_sex_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "missing_sex_1")
  
  nrow(joined_output_2 <-
         datapackcommons::MapDimToOptions(sample_data, missing_sex_set, "replicate")) %>%
    testthat::expect_equal(nrow(sample_data) * 2)

  # Testing all the values being generated in the output column -> "Value" and making sure they're not being multiplied by weights
  # isTRUE(all.equal) returns true if all of the cells in Value are the same as output$sex_weight * value, else it returns false
  testthat::expect_false(isTRUE(all.equal(joined_output_2$Value, joined_output_2$sex_weight * value)))
  
  testthat::expect_error(joined_output_3 <- datapackcommons::MapDimToOptions(sample_data, datapackcommons::dim_item_sets, "distribute"))
  
  # Selecting 0 rows from dimension item sets to be sent to a test
  no_data_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "no data")
  
  # Send the 0 row dataframe to MapDimToOpsions and making sure we get the Sample data back
  joined_no_items_to_options <- datapackcommons::MapDimToOptions(sample_data, no_data_set, "distribute")
  
  testthat::expect_equal(sample_data, joined_no_items_to_options)
})

test_that("MapMechToMech", {
  
  mechanism_map <- tibble::tribble(~psnuid, ~`Technical Area`, ~`Numerator / Denominator`,
                                   ~`Support Type`, ~oldMech, ~newMech, ~weight,
                                   "aaaaaaaaaaa", "PMTCT_STAT", "D", "DSD", "10020", "10270", .7,
                                   "aaaaaaaaaaa", "PMTCT_STAT", "D", "DSD", "12345", "54321", .7,
                                   "aaaaaaaaaaa", "PMTCT_STAT", "D", "DSD", "10020", "10271", .3,
                                   "aaaaaaaaaaa", "PMTCT_STAT", "D", "DSD", "10020", "10270", .5,
                                   "aaaaaaaaaaa", "PMTCT_STAT", "D", "DSD", "10020", "10271", .5,
                                   "aaaaaaaaaaa", "OVC_SERV", "N", "DSD", "10020", "10271", 1,
                                   "aaaaaaaaaaa", "(ALL)", NA_character_, "DSD", "10020", "10271", 1)
  
  density_test <- structure(list(`Age: Cascade Age bands` = "QOawCj9oLNS", `Disaggregation Type` = "Qbz6SrpmJ1y", 
                                       `Cascade sex` = "ZOYVg7Hosni", `Numerator / Denominator` = "QpNj0nSuEhD", 
                                       `Technical Area` = "BTIqHnjeG7l", `Type of organisational unit` = "POHZmzofoVx", 
                                       `Organisation unit` = "bbbbbbbbbbb", `Funding Mechanism` = "ccccccccccc", 
                                       `Support Type` = "iM13vdNLWKb", siteValueH = 76, age_dim_uid = "e485zBiR7vG", 
                                       age_dim_name = "Age: Cascade Age bands", age_dim_cop_type = "age", 
                                       age_dim_item_name = "15-19 (Specific)", age_option_name = "15-19", 
                                       age_option_uid = "ttf9eZCHsTU", age_sort_order = 60, age_weight = 1, 
                                       age_model_sets = "10-50+", sex_dim_uid = "jyUTj5YC3OK", sex_dim_name = "Cascade sex", 
                                       sex_dim_cop_type = "sex", sex_dim_item_name = "Females", 
                                       sex_option_name = "Female", sex_option_uid = "Z1EnpTPaUfq", 
                                       sex_sort_order = 201, sex_weight = 1, sex_model_sets = "F", 
                                       psnuid = "aaaaaaaaaaa", psnuValueH = 2290, mechanismCode = "10020", 
                                       percent = 0.0331877729257642, indicatorCode = "PMTCT_STAT.D.Age/Sex.20T", 
                                       psnuValueH_after_site_drop = NA), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))                                                                                                                                                   

  # Tests for one to two mapping
  
  mapped_output <- MapMechToMech(density_test, mechanism_map)
  
  testthat::expect_equal(nrow(mapped_output), 2)
  testthat::expect_equal(mapped_output$mechanismCode, c("10270","10271"))
  
  # Tests for one to one mapping
  
  mechanism_map_one <- mechanism_map[1,]
  mapped_output_one <- MapMechToMech(density_test, mechanism_map_one)
  testthat::expect_equal(nrow(mapped_output_one), 1)
  testthat::expect_equal(mapped_output_one$mechanismCode, "10270")
  
  # Two to multi mapping
  
  second_row <- density_test
  second_row$mechanismCode = "12345"
  density_test_two <- rbind(density_test, second_row)
  mapped_output_two <- MapMechToMech(density_test_two, mechanism_map)
  
  testthat::expect_equal(mapped_output_two$mechanismCode, c("10270","10271","54321"))
  
  # Test that if no mech to mech map dataframe is passed, only the density is returned
  testthat::expect_equal(MapMechToMech(density_test), density_test)
  
  # Test that if inout site density has no rows, the empty site density df will be passed back
  density_test_no_rows <- density_test[0, ]
  testthat::expect_equal(MapMechToMech(density_test_no_rows, mechanism_map), density_test_no_rows)
  
})
