library(DSLite)
library(DSI)
library(dplyr)
library(dsTidyverse)
library(dsBaseClient)

mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

login_data <- .prepare_dslite(
  aggregate_method = "groupKeysDS",
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group
  )
)

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars_group", "mtcars_group")
datashield.assign.table(conns, "mtcars_bad_group", "mtcars_bad_group")

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
