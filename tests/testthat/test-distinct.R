library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsTidyverseClient)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("distinctDS", "dsTidyverse::distinctDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
builder <- DSI::newDSLoginBuilder()
builder$append(server="test", url="dslite.server", driver = "DSLiteDriver")
login_data <- builder$build()
conns <- datashield.login(logins = login_data)

datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.distinct correctly identified distinct rows", {
  ds.distinct(
    df.name = "mtcars",
    expr = list(cyl, carb),
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.class("dist_df")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("dist_df")[[1]],
    c(9, 2))

})

test_that("ds.distinct works with where `expr` arg is empty", {
  ds.distinct(
    df.name = "mtcars",
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.dim("dist_df")[[1]],
    c(32, 11)
  )

  expect_equal(
    ds.colnames("dist_df")[[1]],
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
    ds.dim("dist_df")[[1]],
    c(9, 11)
  )

  expect_equal(
    ds.colnames("dist_df")[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  ds.distinct(
    df.name = "mtcars",
    expr = list(cyl, carb),
    .keep_all = FALSE,
    newobj = "dist_df",
    datasources = conns)

  expect_equal(
    ds.dim("dist_df")[[1]],
    c(9, 2)
  )

  expect_equal(
    ds.colnames("dist_df")[[1]],
    c("cyl", "carb")
  )

})
