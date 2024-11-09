require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

mtcars <- mtcars %>% mutate(cat_var = factor(ifelse(mpg > 20, "high", "low")))
login_data <- .prepare_dslite(assign_method = "caseWhenDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.case_when passes and numeric condition and numeric output", {
  skip_if_not_installed("dsBaseClient")
  ds.case_when(
    tidy_expr = list(
      mtcars$mpg < 20 ~ "low",
      mtcars$mpg >= 20 & mtcars$mpg < 30 ~ "medium",
      mtcars$mpg >= 30 ~ "high"
    ),
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "low", "medium", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "character"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts, datasources = conns),
    c(12, 54, 30, 0)
  )
})

test_that("ds.case_when correctly passes argument with numeric condition and numeric outcome", {
  skip_if_not_installed("dsBaseClient")
  ds.case_when(
    tidy_expr = list(
      mtcars$mpg < 20 ~ 20,
      mtcars$mpg >= 20 & mtcars$mpg < 30 ~ 30,
      mtcars$mpg >= 30 ~ 40
    ),
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("20", "30", "40", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "numeric"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts, datasources = conns),
    c(54, 30, 12, 0)
  )
})


test_that("ds.case_when correctly passes argument with categorical condition and categorical outcome", {
  skip_if_not_installed("dsBaseClient")
  ds.case_when(
    tidy_expr = list(
      mtcars$gear == 2 ~ "low",
      mtcars$gear == 3 ~ "medium",
      mtcars$gear == 4 ~ "high",
      mtcars$gear == 5 ~ "very_high"
    ),
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "medium", "very_high", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "character"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts, datasources = conns),
    c(36, 45, 15, 0)
  )
})

test_that("ds.case_when correctly passes .default argument", {
  skip_if_not_installed("dsBaseClient")
  ds.case_when(
    tidy_expr = list(
      mtcars$gear == 2 ~ "low",
      mtcars$gear == 3 ~ "medium",
      mtcars$gear == 5 ~ "very_high"
    ),
    .default = "something_missing",
    newobj = "test",
    datasources = conns
  )

  nqmes <- names(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("medium", "something_missing", "very_high", "NA")
  )

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "character"
  )

  counts <- ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts, datasources = conns),
    c(45, 36, 15, 0)
  )
})
