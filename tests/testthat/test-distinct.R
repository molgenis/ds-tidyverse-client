library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsTidyverseClient)
library(dsBase)
library(dsBaseClient)

login_data <- .prepare_dslite(assign_method = "distinctDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.distinct correctly identified distinct rows", {
  ds.distinct(
    df.name = "mtcars",
    expr = list(cyl, carb),
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.class("dist_df", datasources = conns)[[1]],
    "data.frame")

  expect_equal(
    ds.dim("dist_df", datasources = conns)[[1]],
    c(9, 2))

})

test_that("ds.distinct works with where `expr` arg is empty", {
  ds.distinct(
    df.name = "mtcars",
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.dim("dist_df", datasources = conns)[[1]],
    c(32, 11)
  )

  expect_equal(
    ds.colnames("dist_df", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

})

test_that("ds.distinct works with `.keep_all` argument", {
  ds.distinct(
    df.name = "mtcars",
    expr = list(cyl, carb),
    .keep_all = TRUE,
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.dim("dist_df", datasources = conns)[[1]],
    c(9, 11)
  )

  expect_equal(
    ds.colnames("dist_df", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  ds.distinct(
    df.name = "mtcars",
    expr = list(cyl, carb),
    .keep_all = FALSE,
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.dim("dist_df", datasources = conns)[[1]],
    c(9, 2)
  )

  expect_equal(
    ds.colnames("dist_df", datasources = conns)[[1]],
    c("cyl", "carb")
  )

})

test_that("ds.distinct fails if created subset is too small", {

  expect_error(
    ds.distinct(
      df.name = "mtcars",
      expr = list(vs),
      newobj = "dist_df",
      datasources = conns)
  )

})
