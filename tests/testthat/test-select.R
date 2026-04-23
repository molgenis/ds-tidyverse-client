require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data("mtcars")
login_data <- .prepare_dslite(assign_method = "selectDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.select fails with correct error message if data not present ", {
  skip_if_not_installed("dsBaseClient")
  expect_error(
    ds.select(
      df.name = "datanotthere",
      tidy_expr = list(mpg:drat),
      newobj = "nodata",
      datasources = conns
    )
  )
})

test_that("ds.select correctly passes : ", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(mpg:drat),
    newobj = "mpg_drat",
    datasources = conns
  )

  expect_equal(
    ds.colnames("mpg_drat", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat")
  )
})

test_that("ds.select correctly passes strings with '&'", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(starts_with("c") & ends_with("b")),
    newobj = "and",
    datasources = conns
  )

  expect_equal(
    ds.colnames("and", datasources = conns)[[1]],
    "carb"
  )
})

test_that("ds.select correctly passes strings with '!'", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(!mpg),
    newobj = "not",
    datasources = conns
  )

  expect_equal(
    ds.colnames("not", datasources = conns)[[1]],
    c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("ds.select correctly passes strings with '|'", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(starts_with("c") | ends_with("b")),
    newobj = "or",
    datasources = conns
  )

  expect_equal(
    ds.colnames("or", datasources = conns)[[1]],
    c("cyl", "carb")
  )
})

test_that("ds.select correctly passes complex strings", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list((starts_with("c") & ends_with("b")) | contains("ra") | gear:carb),
    newobj = "complex",
    datasources = conns
  )

  expect_equal(
    ds.colnames("complex", datasources = conns)[[1]],
    c("carb", "drat", "gear")
  )
})

test_that("ds.select correctly passes strings with '='", {
  skip_if_not_installed("dsBaseClient")
  ds.select(
    df.name = "mtcars",
    tidy_expr = list(test = mpg, cyl, gear),
    newobj = "equals",
    datasources = conns
  )

  expect_equal(
    ds.colnames("equals", datasources = conns)[[1]],
    c("test", "cyl", "gear")
  )
})
