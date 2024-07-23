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
dslite.server$assignMethod("bindColsDS", "dsTidyverse::bindColsDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

test_that("ds.bind_cols binds two data frames together", {
  ds.bind_cols(
    to_combine = list(mtcars, mtcars),
    newobj = "cols_bound",
    datasources = conns)

  expect_equal(
    ds.class("cols_bound")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("cols_bound")[[1]],
    c(32, 22))

  expect_equal(
    ds.colnames("cols_bound")[[1]],
    c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
      "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
      "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
  )

})

test_that("ds.bind_cols works with .namerepair arg", {
  ds.bind_cols(
    to_combine = list(mtcars, mtcars),
    .name_repair = "universal",
    newobj = "cols_bound",
    datasources = conns)

  expect_equal(
    ds.colnames("cols_bound")[[1]],
    c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
      "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
      "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
  )

  ds.bind_cols(
    to_combine = list(mtcars, mtcars),
    .name_repair = "minimal",
    newobj = "cols_bound",
    datasources = conns)

  expect_equal(
    ds.colnames("cols_bound")[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb",
      "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

})

test_that("ds.bind_cols throws error with .namerepair as 'check_unique'", {
  expect_error(
    ds.bind_cols(
      to_combine = list(mtcars, mtcars),
      .name_repair = "check_unique",
      newobj = "cols_bound",
      datasources = conns)
  )
})

