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