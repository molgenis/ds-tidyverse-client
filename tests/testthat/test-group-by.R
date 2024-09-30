library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

login_data <- .prepare_dslite(
  assign_method = "groupByDS",
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group
  )
)

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "mtcars_group", "mtcars_group")

test_that("ds.group_by correctly groups a data frame", {
  ds.group_by(
    df.name = "mtcars",
    expr = list(mpg, cyl),
    newobj = "grouped",
    datasources = conns
  )

  expect_equal(
    ds.class("grouped", datasources = conns)[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
}) ## Not currently possible to test ,add and .drop arguments due to issues with dslite. Tested in serverside tests

login_data <- .prepare_dslite(
  assign_method = "ungroupDS",
  tables = list(mtcars_group = mtcars_group)
)

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars_group", "mtcars_group")

test_that("ds.ungroup correctly ungroups a data frame", {
  ds.ungroup("mtcars_group", "ungrouped_df", datasources = conns)

  expect_equal(
    ds.class("ungrouped_df", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )
})
