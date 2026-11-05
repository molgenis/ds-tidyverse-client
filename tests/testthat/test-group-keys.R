require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

login_data <- .prepare_login(aggregate_method = "groupKeysDS")

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars_group", .test_table("mtcars_group"))
datashield.assign.table(conns, "mtcars_bad_group", .test_table("mtcars_bad_group"))

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
