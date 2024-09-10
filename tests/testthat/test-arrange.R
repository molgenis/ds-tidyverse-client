library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("arrangeDS", "dsTidyverse::arrangeDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
builder <- DSI::newDSLoginBuilder()
builder$append(server="test", url="dslite.server", driver = "DSLiteDriver")
login_data <- builder$build()
conns <- datashield.login(logins = login_data)

datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.arrange doesn't return error with correct arguments", {
  ds.arrange(
    df.name = "mtcars",
    expr = list(cyl),
    newobj = "ordered_df",
    datasources = conns)

  expect_equal(
    ds.class("ordered_df")[[1]],
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
    ds.class("ordered_df")[[1]],
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
