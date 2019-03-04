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
  df = tibble::tribble(
    ~ type,
    ~ dim_id,
    ~ item_id,
    ~ other_col,
    "dimension",
    "LFsZ8v5v7rq",
    "CW81uF03hvV",
    "Implementing Partner: AIDSRelief Consortium",
    "dimension",
    "LFsZ8v5v7rq",
    "C6nZpLKjEJr",
    "Implementing Partner: African Medical and Research Foundation",
    "filter",
    "dx",
    "BOSZApCrBni",
    "ART enrollment stage 1",
    "filter",
    "dx",
    "dGdeotKpRed",
    "ART enrollment stage 2",
    "dimension",
    "ou",
    "O6uvpzGd5pu",
    "Bo",
    "filter",
    "pe",
    "THIS_FINANCIAL_YEAR",
    ""
  )
  FormatForApi_Dimensions(df, "type", "dim_id", "item_id") %>%
    testthat::expect_equal(
      "dimension=LFsZ8v5v7rq:CW81uF03hvV;C6nZpLKjEJr&dimension=ou:O6uvpzGd5pu&filter=dx:BOSZApCrBni;dGdeotKpRed&filter=pe:THIS_FINANCIAL_YEAR"
    )
  testthat::expect_error(FormatForApi_Dimensions(df, "type", "dim_id", "Nonsense"))
})

test_that("RenameDimensionColumns", {
  df_1 = tibble::tribble(
    ~ dim_uid,
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
    ~ `Cascade sex`,
    ~ Value,
    "ZOYVg7Hosni",
    value,
    "Gxcf2DK8vNc",
    value,
    "hDBPKTjUPDm",
    value
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
