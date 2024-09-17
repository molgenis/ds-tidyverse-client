library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

mtcars_bad_group <- mtcars %>% group_by(qsec)

dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group,
    mtcars_bad_group = mtcars_bad_group
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("groupByDS", "groupByDS")
dslite.server$assignMethod("ungroupDS", "ungroupDS")
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
conns <- DSI::datashield.login(logins = logindata, assign = TRUE)


datashield.assign.table(
  conns = conns,
  table = "mtcars_group",
  symbol = "mtcars_group"
)

test_that("ds.group_by correctly groups a data frame", {
  ds.group_by(
    df.name = "mtcars",
    expr = list(mpg, cyl),
    newobj = "grouped"
  )

  expect_equal(
    ds.class("grouped")[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
}) ## Not currently possible to test ,add and .drop arguments due to issues with dslite. Tested in serverside tests

test_that("ds.ungroup correctly ungroups a data frame", {
  ds.ungroup("mtcars_group", "ungrouped_df")

  expect_equal(
    ds.class("grouped")[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
})
