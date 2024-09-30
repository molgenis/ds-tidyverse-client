library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

login_data <- .prepare_dslite(assign_method = "arrangeDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.arrange doesn't return error with correct arguments", {
  ds.arrange(
    df.name = "mtcars",
    expr = list(cyl),
    newobj = "ordered_df",
    datasources = conns)

  expect_equal(
    ds.class("ordered_df", datasources = conns)[[1]],
    "data.frame")

}) ## Not possible to test that it has been ordered correctly as cannot see data. Will test serverside.

test_that("ds.arrange doesn't return error with .by_group argument", {
  ds.arrange(
    df.name = "mtcars",
    expr = list(cyl),
    .by_group = TRUE,
    newobj = "ordered_df",
    datasources = conns)

  expect_equal(
    ds.class("ordered_df", datasources = conns)[[1]],
    "data.frame")

})

test_that("ds.arrange returns error if data doesn't exist", {
  expect_error(
    ds.arrange(
      df.name = "doesnt_exist",
      expr = list(cyl),
      newobj = "ordered_df",
      datasources = conns)
    )

})

test_that("ds.arrange works with desc() specification", {

  ds.arrange(
    df.name = "mtcars",
    expr = list(desc(cyl)),
    newobj = "desc_df",
    datasources = conns)

  expect_equal(
    ds.class("desc_df", datasources = conns)[[1]],
    "data.frame"
  )

})
