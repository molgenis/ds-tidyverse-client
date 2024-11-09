require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

login_data <- .prepare_dslite(assign_method = "filterDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.filter correctly filters", {
  skip_if_not_installed("dsBaseClient")
  ds.filter(
    df.name = "mtcars",
    tidy_expr = list(cyl == 4 & mpg > 20),
    newobj = "filtered",
    datasources = conns
  )

  expect_equal(
    ds.class("filtered", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("filtered", datasources = conns)[[1]],
    c(11, 11)
  )

  expect_equal(
    ds.mean("filtered$mpg", datasources = conns)$Mean.by.Study[[1]],
    26.66364,
    tolerance = 0.001
  )
})

test_that("ds.filter works with .by arg", {
  skip_if_not_installed("dsBaseClient")
  ds.filter(
    df.name = "mtcars",
    tidy_expr = list(mpg > median(mpg)),
    .by = "cyl",
    newobj = "filtered_by",
    datasources = conns
  )

  expect_equal(
    ds.dim("filtered_by", datasources = conns)[[1]],
    c(14, 11)
  )

  expect_equal(
    ds.mean("filtered_by$mpg", datasources = conns)$Mean.by.Study[[1]],
    22.90714,
    tolerance = 0.00001
  )
})

test_that("ds.filter works with .preserve arg", {
  skip_if_not_installed("dsBaseClient")
  ds.filter(
    df.name = "mtcars",
    tidy_expr = list(mpg > median(mpg)),
    .preserve = T,
    newobj = "preserved_t",
    datasources = conns
  )

  expect_equal(
    ds.class("preserved_t", datasources = conns)[[1]],
    "data.frame"
  )

  ds.filter(
    df.name = "mtcars",
    tidy_expr = list(mpg > median(mpg)),
    .preserve = F,
    newobj = "preserved_f",
    datasources = conns
  )

  expect_equal(
    ds.class("preserved_f", datasources = conns)[[1]],
    "data.frame"
  )
})
