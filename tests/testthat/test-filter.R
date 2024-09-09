library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("filterDS", "dsTidyverse::filterDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
builder <- DSI::newDSLoginBuilder()
builder$append(server = "test", url = "dslite.server", driver = "DSLiteDriver")
login_data <- builder$build()
conns <- datashield.login(logins = login_data)

datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.filter correctly filters", {
  ds.filter(
    df.name = "mtcars",
    expr = list(cyl == 4 & mpg > 20),
    newobj = "filtered",
    datasources = conns
  )

  expect_equal(
    ds.class("filtered")[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("filtered")[[1]],
    c(11, 11)
  )

  expect_equal(
    ds.mean("filtered$mpg")$Mean.by.Study[[1]],
    26.66364,
    tolerance = 0.001
  )
})

test_that("ds.filter works with .by arg", {
  ds.filter(
    df.name = "mtcars",
    expr = list(mpg > median(mpg)),
    .by = "cyl",
    newobj = "filtered_by",
    datasources = conns
  )

  expect_equal(
    ds.dim("filtered_by")[[1]],
    c(14, 11)
  )

  expect_equal(
    ds.mean("filtered_by$mpg")$Mean.by.Study[[1]],
    22.90714,
    tolerance = 0.00001
  )
})

test_that("ds.filter works with .preserve arg", {
  ds.filter(
    df.name = "mtcars",
    expr = list(mpg > median(mpg)),
    .preserve = T,
    newobj = "preserved_t",
    datasources = conns
  )

  expect_equal(
    ds.class("preserved_t")[[1]],
    "data.frame"
  )

  ds.filter(
    df.name = "mtcars",
    expr = list(mpg > median(mpg)),
    .preserve = F,
    newobj = "preserved_f",
    datasources = conns
  )

  expect_equal(
    ds.class("preserved_f")[[1]],
    "data.frame"
  ) # Currently not possible to really test this from the clientside because I haven't implemented the function `group_keys` to return the groups
})
