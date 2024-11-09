require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

mtcars <- mtcars %>% mutate(cat_var = factor(ifelse(mpg > 20, "high", "low")))
login_data <- .prepare_dslite(assign_method = "ifElseDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.if_else correctly passes argument with numeric condition and categorical outcome", {
  skip_if_not_installed("dsBaseClient")
  ds.if_else(
    condition = list(mtcars$mpg > 20),
    "high",
    "low",
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "low", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "character"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )
})

test_that("ds.if_else correctly passes argument with numeric condition and numerical outcome", {
  skip_if_not_installed("dsBaseClient")
  ds.if_else(
    condition = list(mtcars$mpg > 20),
    99,
    100,
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("99", "100", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "numeric"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )
})

test_that("ds.if_else correctly passes argument with = ", {
  skip_if_not_installed("dsBaseClient")
  ds.if_else(
    condition = list(mtcars$vs == "0"),
    "no",
    "yes",
    newobj = "testcat",
    datasources = conns
  )

  names <- names(ds.table("testcat", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    names,
    c("no", "yes", "NA")
  )

  expect_equal(
    ds.class("testcat", datasources = conns)[[1]],
    "character"
  )

  counts <- ds.table("testcat", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(54, 42, 0)
  )
})
