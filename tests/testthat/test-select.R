library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

data("mtcars")
login_data <- .prepare_dslite(assign_method = "bindRowsDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.select fails with correct error message if data not present ", {
  expect_error(
    ds.select(
      df.name = "datanotthere",
      tidy_select = list(mpg:drat),
      newobj = "nodata",
      datasources = conns)
    )
})

test_that("ds.select correctly passes : ", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(mpg:drat),
    newobj = "mpg_drat",
    datasources = conns
  )

  expect_equal(
    ds.colnames("mpg_drat", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat")
  )

})

test_that("ds.select correctly passes `starts_with`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("m")),
    newobj = "starts",
    datasources = conns
  )

  expect_equal(
    ds.colnames("starts", datasources = conns)[[1]],
    "mpg"
  )

})

test_that("ds.select correctly passes `ends_with`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(ends_with("m")),
    newobj = "ends",
    datasources = conns
  )

  expect_equal(
    ds.colnames("ends", datasources = conns)[[1]],
    "am"
  )

})

test_that("ds.select correctly passes `matches`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(matches("[aeiou]")),
    newobj = "matches",
    datasources = conns
  )

  expect_equal(
    ds.colnames("matches", datasources = conns)[[1]],
    c("disp", "drat", "qsec", "am", "gear", "carb")
  )

})

test_that("ds.select correctly passes `everything`", {
  tryCatch(
  ds.select(
    df.name = "mtcars",
    tidy_select = list(everything()),
    newobj = "everything",
    datasources = conns
  ),
  error = function(e){print(datashield.errors)}
  )

  expect_equal(
    ds.colnames("everything", datasources = conns)[[1]],
    colnames(mtcars)
  )

})

test_that("ds.select correctly passes `last_col`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(last_col()),
    newobj = "last",
    datasources = conns
  )

  expect_equal(
    ds.colnames("last", datasources = conns)[[1]],
    "carb"
  )

})

test_that("ds.select correctly passes `group_cols`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(group_cols()),
    newobj = "group",
    datasources = conns
  )

  expect_equal(
    ds.colnames("group", datasources = conns)[[1]],
    character(0)
  )

})

test_that("ds.select correctly passes strings with '&'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("c") & ends_with("b")),
    newobj = "and",
    datasources = conns
  )

  expect_equal(
    ds.colnames("and", datasources = conns)[[1]],
    "carb"
  )

})

test_that("ds.select correctly passes strings with '!'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(!mpg),
    newobj = "not",
    datasources = conns
  )

  expect_equal(
    ds.colnames("not", datasources = conns)[[1]],
    c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

})

test_that("ds.select correctly passes strings with '|'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("c") | ends_with("b")),
    newobj = "or",
    datasources = conns
  )

  expect_equal(
    ds.colnames("or", datasources = conns)[[1]],
    c("cyl", "carb")
  )

})

test_that("ds.select correctly passes `strings with `all_of`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(all_of(c("mpg", "cyl"))),
    newobj = "all_of",
    datasources = conns
  )

  expect_equal(
    ds.colnames("all_of", datasources = conns)[[1]],
    c("mpg", "cyl")
  )

})

test_that("ds.select correctly passes strings with `any_of`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(any_of(c("mpg", "cyl"))),
    newobj = "any_of",
    datasources = conns
  )

  expect_equal(
    ds.colnames("any_of", datasources = conns)[[1]],
    c("mpg", "cyl")
  )

})

test_that("ds.select correctly passes complex strings", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list((starts_with("c") & ends_with("b")) | contains("ra") | gear:carb),
    newobj = "complex",
    datasources = conns
  )

  expect_equal(
    ds.colnames("complex", datasources = conns)[[1]],
    c("carb", "drat", "gear")
  )

})

test_that("ds.select correctly passes strings with `where`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(where(is.numeric)),
    newobj = "where",
    datasources = conns
  )

  expect_equal(
    ds.colnames("where", datasources = conns)[[1]],
    colnames(mtcars)
  )

})

test_that("ds.select correctly passes strings with '='", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(test = mpg, cyl, gear),
    newobj = "equals",
    datasources = conns
  )

  expect_equal(
    ds.colnames("equals", datasources = conns)[[1]],
    c("test", "cyl", "gear")
  )

})
