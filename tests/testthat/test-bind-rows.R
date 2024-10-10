library(DSLite)
library(DSI)
library(dplyr)
library(dsTidyverse)
library(dsBaseClient)

login_data <- .prepare_dslite(assign_method = "bindRowsDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.bind_rows binds two data frames together", {
  skip_if_not_installed("dsBaseClient")
  ds.bind_rows(
    to_combine = list(mtcars, mtcars),
    newobj = "newnew",
    .id = NULL,
    datasources = conns
  )

  expect_equal(
    ds.class("newnew", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("newnew", datasources = conns)[[1]],
    c(64, 11)
  )

  expect_equal(
    ds.colnames("newnew", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("ds.case_when works with .id arg", {
  skip_if_not_installed("dsBaseClient")
  ds.bind_rows(
    to_combine = list(mtcars, mtcars),
    .id = "which_df",
    newobj = "test",
    datasources = conns
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("test", datasources = conns)[[1]],
    c(64, 12)
  )

  expect_equal(
    ds.colnames("test", datasources = conns)[[1]],
    c("which_df", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})
