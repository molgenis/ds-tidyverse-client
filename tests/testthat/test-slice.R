library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars_group <- mtcars %>%
  group_by(carb) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("sliceDS", "sliceDS")
dslite.server$aggregateMethod("groupKeysDS", "groupKeysDS")
dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server = "server_1",
  url = "dslite.server",
  table = "mtcars",
  driver = "DSLiteDriver"
)

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = FALSE)

datashield.assign.table(
  conns = conns,
  table = "mtcars",
  symbol = "mtcars"
)

datashield.assign.table(
  conns = conns,
  table = "mtcars_group",
  symbol = "mtcars_group"
)

test_that("ds.slice correctly subsets rows", {
  ds.slice(
    df.name = "mtcars",
    expr = list(1:5),
    newobj = "sliced",
    datasources = conns)

  expect_equal(
    ds.class("sliced")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("sliced")[[1]],
    c(5, 11))

})

test_that("ds.slice works with .by arg", {
  ds.slice(
    df.name = "mtcars",
    expr = list(1:5),
    .by = "cyl",
    newobj = "sliced_by",
    datasources = conns)

  expect_equal(
    ds.dim("sliced_by")[[1]],
    c(15, 11)
  )

}) ## Currently not possible to test .preserve clientside because ds.group_keys not in this PR. Will
## test serverside

test_that("ds.slice throws error if disclosure risk", {

  expect_error(
    ds.slice(
      df.name = "mtcars",
      expr = list(1),
      newobj = "sliced_by",
      datasources = conns)
  )

})
