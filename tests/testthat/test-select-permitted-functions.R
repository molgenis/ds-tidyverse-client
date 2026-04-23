require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data("mtcars")
num_range_df <- data.frame(x1 = 1:5, x2 = 6:10, x3 = 11:15, y1 = 16:20, stringsAsFactors = FALSE)
login_data <- .prepare_dslite(assign_method = "selectDS", tables = list(mtcars = mtcars, num_range_df = num_range_df))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "num_range_df", "num_range_df")

test_that("ds.select correctly passes `starts_with`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(starts_with("m")),
    newobj = "starts",
    datasources = conns
  )

  expect_equal(
    ds.colnames("starts", datasources = conns)[[1]],
    "mpg"
  )
})

test_that("ds.select correctly passes `ends_with`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(ends_with("m")),
    newobj = "ends",
    datasources = conns
  )

  expect_equal(
    ds.colnames("ends", datasources = conns)[[1]],
    "am"
  )
})

test_that("ds.select correctly passes `matches`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(matches("a")),
    newobj = "matches",
    datasources = conns
  )

  expect_equal(
    ds.colnames("matches", datasources = conns)[[1]],
    c("drat", "am", "gear", "carb")
  )
})

test_that("ds.select correctly passes `everything`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(everything()),
    newobj = "everything",
    datasources = conns
  )

  expect_equal(
    ds.colnames("everything", datasources = conns)[[1]],
    colnames(mtcars)
  )
})

test_that("ds.select correctly passes `last_col`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(last_col()),
    newobj = "last",
    datasources = conns
  )

  expect_equal(
    ds.colnames("last", datasources = conns)[[1]],
    "carb"
  )
})

test_that("ds.select correctly passes `group_cols`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(group_cols()),
    newobj = "group",
    datasources = conns
  )

  expect_equal(
    ds.colnames("group", datasources = conns)[[1]],
    character(0)
  )
})

test_that("ds.select correctly passes `all_of`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(all_of(c("mpg", "cyl"))),
    newobj = "all_of",
    datasources = conns
  )

  expect_equal(
    ds.colnames("all_of", datasources = conns)[[1]],
    c("mpg", "cyl")
  )
})

test_that("ds.select correctly passes `any_of`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(any_of(c("mpg", "cyl"))),
    newobj = "any_of",
    datasources = conns
  )

  expect_equal(
    ds.colnames("any_of", datasources = conns)[[1]],
    c("mpg", "cyl")
  )
})

test_that("ds.select correctly passes `contains`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(contains("ra")),
    newobj = "contains",
    datasources = conns
  )

  expect_equal(
    ds.colnames("contains", datasources = conns)[[1]],
    "drat"
  )
})

test_that("ds.select correctly passes `where`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(where(is.numeric)),
    newobj = "where",
    datasources = conns
  )

  expect_equal(
    ds.colnames("where", datasources = conns)[[1]],
    colnames(mtcars)
  )
})

test_that("ds.select correctly passes `num_range`", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "num_range_df",
    tidy_expr = list(num_range("x", 1:2)),
    newobj = "num_range_res",
    datasources = conns
  )

  expect_equal(
    ds.colnames("num_range_res", datasources = conns)[[1]],
    c("x1", "x2")
  )
})
