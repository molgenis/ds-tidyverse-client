require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

login_data <- .prepare_login(assign_method = "groupByDS")

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", .test_table("mtcars"))
datashield.assign.table(conns, "mtcars_drop_test", .test_table("mtcars_drop_test"))
ds.group_by("mtcars_drop_test", tidy_expr = list(cyl), newobj = "mtcars_group", datasources = conns)

test_that("ds.group_by correctly groups a data frame", {
  skip_if_not_installed("dsBaseClient")
  ds.group_by(
    df.name = "mtcars",
    tidy_expr = list(mpg, cyl),
    newobj = "grouped",
    datasources = conns
  )

  expect_equal(
    ds.class("grouped", datasources = conns)[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
})

login_data <- .prepare_login(assign_method = "ungroupDS")

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars_drop_test", .test_table("mtcars_drop_test"))
ds.group_by("mtcars_drop_test", tidy_expr = list(cyl), newobj = "mtcars_group", datasources = conns)

test_that("ds.ungroup correctly ungroups a data frame", {
  skip_if_not_installed("dsBaseClient")
  ds.ungroup("mtcars_group", "ungrouped_df", datasources = conns)

  expect_equal(
    ds.class("ungrouped_df", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )
})
