#' Check Columns as Expected
#'
#' Helper function for test files.
#'
#' @param expected A character vector of expected column names.
#' @param df A dataframe whose columns are to be checked.
#' @return This function does not return a value. It is used for its side effect of throwing an
#' error if the observed column names do not match the expected ones.
#' @importFrom dsBaseClient ds.colnames
#' @importFrom testthat expect_equal
#' @noRd
.check_cols_as_expected <- function(expected, df) {
  observed <- ds.colnames(df)[[1]]
  expect_equal(observed, expected)
}
