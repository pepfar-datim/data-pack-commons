context("Test helper utility functions")

test_that("StackPrefixedCols", {
  data  <-  data.frame(
    A.x = c(1, 1),
    B.x = c(2, 2),
    A.A.x = c(3, 3),
    B.z = c(100, 100),
    b.extra = c(100, 100)
  )
  prefixes <- c("A.", "B.")
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
  df <- tibble::tribble(~ type, ~ dim_id, ~ item_id, ~ other_col,
    "dimension", "LFsZ8v5v7rq", "CW81uF03hvV", "Implementing Partner: AIDSRelief Consortium",
    "dimension", "LFsZ8v5v7rq", "C6nZpLKjEJr", "Implementing Partner: African Medical and Research Foundation",
    "filter", "dx", "BOSZApCrBni", "ART enrollment stage 1",
    "filter", "dx", "dGdeotKpRed", "ART enrollment stage 2",
    "dimension", "ou", "O6uvpzGd5pu", "Bo",
    "filter", "pe", "THIS_FINANCIAL_YEAR", ""
    )
  FormatForApi_Dimensions(df, "type", "dim_id", "item_id") %>%
    testthat::expect_equal(
      paste0("dimension=LFsZ8v5v7rq:CW81uF03hvV;C6nZpLKjEJr&dimension=ou:O6uvpzGd5pu&",
             "filter=dx:BOSZApCrBni;dGdeotKpRed&filter=pe:THIS_FINANCIAL_YEAR")
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
  df_2 <- tibble::tribble(
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
  # Tue Aug 23 09:55:58 2022 - The model set"F/M/U" was replaced with F/M.
  sex_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "F/M") #"F/M/U"


  unique(sex_set$dim_name) %>%
    testthat::expect_equal("Cascade sex")

  joined_output_1 <-
    datapackcommons::MapDimToOptions(sample_data, sex_set, "distribute")
  joined_output_1$Value %>%
    testthat::expect_equal(joined_output_1$sex_weight * value)

  nrow(joined_output_1 %>%
         dplyr::filter((sex_dim_item_name == "Females"))) %>%
    testthat::expect_equal(1) # "Unspecified sex" above and 2 here


  missing_sex_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "missing_sex_1")

  nrow(joined_output_2 <-
         datapackcommons::MapDimToOptions(sample_data, missing_sex_set, "replicate")) %>%
    testthat::expect_equal(nrow(sample_data) * 2)

  # Testing all the values being generated in the output column -> "Value" and
  # making sure they're not being multiplied by weights
  # isTRUE(all.equal) returns true if all of the cells in Value are
  # the same as output$sex_weight * value, else it returns false
  testthat::expect_false(isTRUE(all.equal(joined_output_2$Value, joined_output_2$sex_weight * value)))

  testthat::expect_error(joined_output_3 <- datapackcommons::MapDimToOptions(sample_data,
                                                                             datapackcommons::dim_item_sets,
                                                                             "distribute"))

  # Selecting 0 rows from dimension item sets to be sent to a test
  no_data_set <- datapackcommons::dim_item_sets %>%
    dplyr::filter(model_sets == "no data")

  # Send the 0 row dataframe to MapDimToOpsions and making sure we get the Sample data back
  joined_no_items_to_options <- datapackcommons::MapDimToOptions(sample_data, no_data_set, "distribute")

  testthat::expect_equal(sample_data, joined_no_items_to_options)
})

# test psnuxim model differences ----
library(dplyr)

httptest::use_mock_api()
test_that("Can compare psnuxim model data", {

  play230 <- list(base_url = "https://play.dhis2.org/2.30/",
                            handle = httr::handle("https://play.dhis2.org/2.30/"))

  # list of countries lets say of new model pulled
  country_details <-
    tribble(
      ~country_name, ~id,
      "Angola", "XOivy2uDpMF",
      "Antigua and Barbuda", "CDGPst7p3vc"
    )

  # new model data
  new_model <- list(
    XOivy2uDpMF = tribble(
      ~indicator_code, ~psnu_uid, ~value, ~percent,
      "HTS_INDEX_COM.New.Neg.T", "bSwiBL5Y4SW", 4, .45
    ),
    CDGPst7p3vc = tribble()
  )

  # old model that we "load" in
  old_model <- list(
    XOivy2uDpMF = tribble(
      ~indicator_code, ~psnu_uid, ~value, ~ percent,
      "HTS_INDEX_COM.New.Neg.T", "bSwiBL5Y4SW", 2, .45
    ),
    CDGPst7p3vc = tribble(),
    a71G4Gtcttv = tribble(
      ~indicator_code, ~psnu_uid, ~value, ~percent,
      "CXCA_SCRN.T", "a3zBlSiF0wB", 16, .1
    )
  )

  # PARTIAL DIFF, ONLY SAME COUNTRIES ----
  partial_deltas <- diffSnuximModels(
    model_old = old_model,
    model_new = new_model,
    d2_session = play230,
    full_diff = FALSE)

  testthat::expect_equal(nrow(partial_deltas), 1)
  rm(ancestors_data)

  # FULL DIFF, ALL COUNTRIES ----
   total_diff <- diffSnuximModels(
     model_old = old_model,
     model_new = new_model,
     d2_session = play230,
     full_diff = TRUE)
   testthat::expect_equal(nrow(total_diff), 2)

})

# test data entry form differences ----

test_that("Can compare dataframes", {

  # form a
  a <-
    data.frame(
      A.dataset = c(1, 2, 3, 4, 5, 6, 7),
      y = c("A", "B", "C", "D", "E", "F", "G")
    )

  # form b
  b <-
    data.frame(
      B.dataset = c(8, 9, 10, 4, 5, 6, 7),
      z = c("H", "I", "J", "D", "E", "F", "G")
    )

  d <- list()

  # test dataframe stop error if param is empty
  testthat::expect_error(
    diffDataFrames(
      dataframe_a = NULL,
      dataframe_b = b
    ),
    "one or both of your dataframe values are empty!"
  )

  # test dataframe stop error
  testthat::expect_error(
    diffDataFrames(
      dataframe_a = d,
      dataframe_b = b
    ),
    "one or both of these are not dataframes!"
  )

  # test stop error if names are unequal
  testthat::expect_error(
    diffDataFrames(
      dataframe_a = a,
      dataframe_b = b
    ),
    "your dataframes seem to have different names!"
  )

  # test positive list result
  names(b)[names(b) == "z"] <- "y"
  res <- diffDataFrames(
    dataframe_a = a,
    dataframe_b = b
  )

  # test output is as expected
  testthat::expect_equal(
    res,
    list(
      "a_not_b" =
        data.frame(
          A.dataset = c(1, 2, 3),
          y = c("A", "B", "C")
        ),
      "b_not_a" =
        data.frame(
          B.dataset = c(8, 9, 10),
          y = c("H", "I", "J")
        ),
      "a_and_b" =
        data.frame(
          A.dataset = c(4, 5, 6, 7),
          y = c("D", "E", "F", "G"),
          B.dataset = c(4, 5, 6, 7)
        )
    )
  )

  # test return null when join fails
  a$y <- c(6, 7, 1, 3, 5, 6, 8) # num vals crash join
  res <- diffDataFrames(
    dataframe_a = a,
    dataframe_b = b
  )
  testthat::expect_null(unique(res)[[1]])

})
