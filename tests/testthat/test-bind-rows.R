library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("bindRowsDS", "dsTidyverse::bindRowsDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

test_that("ds.bind_rows binds two data frames together", {
  ds.bind_rows(
    to_combine = list(mtcars, mtcars),
    newobj = "newnew",
    .id = NULL,
    datasources = conns)

  expect_equal(
    ds.class("newnew")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("newnew")[[1]],
    c(64, 11))

  expect_equal(
    ds.colnames("newnew")[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))

})

test_that("ds.case_when works with .id arg", {
  ds.bind_rows(
    to_combine = list(mtcars, mtcars),
    .id = "which_df",
    newobj = "test",
    datasources = conns)

  expect_equal(
    ds.class("test")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("test")[[1]],
    c(64, 12))

  expect_equal(
    ds.colnames("test")[[1]],
    c("which_df", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))

})
