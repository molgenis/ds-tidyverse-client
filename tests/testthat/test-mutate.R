require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data(mtcars)
login_data <- .prepare_dslite(assign_method = "mutateDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.mutate correctly passes good argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate(
    df.name = "mtcars",
    tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
    newobj = "new",
    datasources = conns
  )
  expected_cols <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "mpg_trans", "new_var")

  expect_equal(
    ds.colnames("new", datasources = conns)[[1]],
    expected_cols
  )

  expected_mean <- ds.mean("new$mpg_trans", datasources = conns)
  expect_equal(
    mean(expected_mean$Mean.by.Study[1]),
    6187.5
  )
})

test_that("ds.mutate fails with bad argument argument", {
  skip_if_not_installed("dsBaseClient")
  expect_error(
    ds.mutate(
      df.name = "mtcars",
      tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec, filterasd("asdasdf")),
      newobj = "new",
      datasources = conns
    )
  )

  expect_error(
    ds.mutate(
      df.name = "mtcars",
      tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
      newobj = "new",
      .keep = NULL,
      datasources = conns
    )
  )
})

test_that("ds.mutate passes with different .keep argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate(
    df.name = "mtcars",
    tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
    newobj = "new",
    .keep = "none",
    .before = NULL,
    .after = NULL,
    datasources = conns
  )

  expect_equal(
    ds.colnames("new", datasources = conns)[[1]],
    c("mpg_trans", "new_var")
  )
})

test_that("ds.mutate passes with different .before argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate(
    df.name = "mtcars",
    tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
    newobj = "new",
    .keep = "all",
    .before = "disp",
    .after = NULL,
    datasources = conns
  )

  expect_equal(
    ds.colnames("new", datasources = conns)[[1]],
    c("mpg", "cyl", "mpg_trans", "new_var", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("ds.mutate passes with different .after argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate(
    df.name = "mtcars",
    tidy_expr = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
    newobj = "new",
    .keep = "all",
    .before = NULL,
    .after = "qsec",
    datasources = conns
  )

  expect_equal(
    ds.colnames("new", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "mpg_trans", "new_var", "vs", "am", "gear", "carb")
  )
})

date_df <- data.frame(date_str = c("2024-01-15", "2024-06-30", "2024-12-25"), stringsAsFactors = FALSE)
date_login_data <- .prepare_dslite(assign_method = "mutateDS", tables = list(date_df = date_df))
date_conns <- datashield.login(logins = date_login_data)
datashield.assign.table(date_conns, "date_df", "date_df")

test_that("ds.mutate passes with as.Date on a character column", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate(
    df.name = "date_df",
    tidy_expr = list(date_col = as.Date(date_str)),
    newobj = "date_result",
    datasources = date_conns
  )

  expect_equal(
    ds.class("date_result$date_col", datasources = date_conns)[[1]],
    "Date"
  )
})
