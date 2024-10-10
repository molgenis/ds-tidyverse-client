require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data("mtcars")
mtcars_group <- mtcars %>%
  group_by(carb) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

login_data <- .prepare_dslite(assign_method = "sliceDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "mtcars_group", "mtcars_group")

test_that("ds.slice correctly subsets rows", {
  skip_if_not_installed("dsBaseClient")
  ds.slice(
    df.name = "mtcars",
    tidy_expr = list(1:5),
    newobj = "sliced",
    datasources = conns
  )

  expect_equal(
    ds.class("sliced", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("sliced", datasources = conns)[[1]],
    c(5, 11)
  )
})

test_that("ds.slice works with .by arg", {
  skip_if_not_installed("dsBaseClient")
  ds.slice(
    df.name = "mtcars",
    tidy_expr = list(1:5),
    .by = "cyl",
    newobj = "sliced_by",
    datasources = conns
  )

  expect_equal(
    ds.dim("sliced_by", datasources = conns)[[1]],
    c(15, 11)
  )
}) ## Currently not possible to test .preserve clientside because ds.group_keys not in this PR. Will
## test serverside

test_that("ds.slice throws error if disclosure risk", {
  skip_if_not_installed("dsBaseClient")
  expect_error(
    ds.slice(
      df.name = "mtcars",
      tidy_expr = list(1),
      newobj = "sliced_by",
      datasources = conns
    )
  )
})
