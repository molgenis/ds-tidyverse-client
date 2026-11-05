require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

login_data <- .prepare_login(aggregate_method = "groupKeysDS")

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", .test_table("mtcars"))
datashield.assign.table(conns, "mtcars_drop_test", .test_table("mtcars_drop_test"))
ds.group_by("mtcars_drop_test", tidy_expr = list(cyl), newobj = "mtcars_group", datasources = conns)
ds.group_by("mtcars", tidy_expr = list(qsec), newobj = "mtcars_bad_group", datasources = conns)

test_that("ds.group_keys correctly returns groups", {
  skip_if_not_installed("dsBaseClient")
  groups <- ds.group_keys("mtcars_group", datasources = conns)

  expect_equal(
    groups[[1]],
    tibble(cyl = c(4, 6, 8))
  )
})

test_that("ds.group_keys returns error if too many groups", {
  skip_if_not_installed("dsBaseClient")
  expect_error(
    ds.group_keys("mtcars_bad_group", datasources = conns)
  )
})
