require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data(mtcars)
pf_na_df <- data.frame(x = c(1, NA, 3, NA, 5), stringsAsFactors = FALSE)
pf_date_df <- data.frame(date_str = c("2024-01-15", "2024-06-30", "2024-12-25"), stringsAsFactors = FALSE)
pf_date_dmy_df <- data.frame(date_str = c("15/01/2024", "30/06/2024", "25/12/2024"), stringsAsFactors = FALSE)
pf_login_data <- .prepare_dslite(
  assign_method = "mutateDS",
  tables = list(mtcars = mtcars, pf_na_df = pf_na_df, pf_date_df = pf_date_df, pf_date_dmy_df = pf_date_dmy_df)
)
conns <- datashield.login(logins = pf_login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "pf_na_df", "pf_na_df")
datashield.assign.table(conns, "pf_date_df", "pf_date_df")
datashield.assign.table(conns, "pf_date_dmy_df", "pf_date_dmy_df")

# Math functions

test_that("ds.mutate permits exp()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = exp(cyl)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(exp(mtcars$cyl)), 2)
  )
})

test_that("ds.mutate permits sqrt()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = sqrt(hp)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(sqrt(mtcars$hp)), 2)
  )
})

test_that("ds.mutate permits round()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = round(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    mean(round(mtcars$mpg))
  )
})

test_that("ds.mutate permits floor()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = floor(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    mean(floor(mtcars$mpg))
  )
})

test_that("ds.mutate permits ceiling()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = ceiling(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    mean(ceiling(mtcars$mpg))
  )
})

test_that("ds.mutate permits abs()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = abs(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(abs(mtcars$mpg)), 2)
  )
})

# Stats functions

test_that("ds.mutate permits mean()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = mean(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(mtcars$mpg), 2)
  )
})

test_that("ds.mutate permits median()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = median(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    median(mtcars$mpg)
  )
})

test_that("ds.mutate permits sd()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = sd(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(sd(mtcars$mpg), 2)
  )
})

test_that("ds.mutate permits var()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = var(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(var(mtcars$mpg), 2)
  )
})

# Trig functions

test_that("ds.mutate permits sin()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = sin(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(sin(mtcars$mpg)), 2)
  )
})

test_that("ds.mutate permits cos()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = cos(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(cos(mtcars$mpg)), 2)
  )
})

test_that("ds.mutate permits tan()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = tan(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(tan(mtcars$mpg)), 2)
  )
})

test_that("ds.mutate permits asin()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = asin(mpg / 34)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(asin(mtcars$mpg / 34)), 2)
  )
})

test_that("ds.mutate permits acos()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = acos(mpg / 34)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(acos(mtcars$mpg / 34)), 2)
  )
})

test_that("ds.mutate permits atan()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = atan(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(atan(mtcars$mpg)), 2)
  )
})

# Type conversion functions

test_that("ds.mutate permits as.character()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = as.character(cyl)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "character"
  )
})

test_that("ds.mutate permits as.integer()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = as.integer(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "integer"
  )
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    mean(as.integer(mtcars$mpg))
  )
})

test_that("ds.mutate permits as.numeric()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = as.numeric(cyl)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "numeric"
  )
  expect_equal(
    ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1],
    mean(as.numeric(mtcars$cyl))
  )
})

test_that("ds.mutate permits as.Date() with ISO format", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("pf_date_df", tidy_expr = list(new_col = as.Date(date_str)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "Date"
  )
})

test_that("ds.mutate permits as.Date() with positional format argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("pf_date_dmy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%d/%m/%Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("15/01/2024", "30/06/2024", "25/12/2024"), "%d/%m/%Y"))), 2)
  )
})

test_that("ds.mutate as.Date() without format gives wrong result for non-ISO dates", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("pf_date_dmy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str))), newobj = "res_wrong", datasources = conns)
  correct_mean <- round(mean(as.numeric(as.Date(c("15/01/2024", "30/06/2024", "25/12/2024"), "%d/%m/%Y"))), 2)
  wrong_mean <- round(ds.mean("res_wrong$date_num", datasources = conns)$Mean.by.Study[1, 1], 2)
  expect_false(wrong_mean == correct_mean)
})

# Window functions

test_that("ds.mutate permits lag()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = lag(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(lag(mtcars$mpg), na.rm = TRUE), 2)
  )
})

test_that("ds.mutate permits cumsum()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = cumsum(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(cumsum(mtcars$mpg)), 2)
  )
})

# Logical and conditional functions

test_that("ds.mutate permits is.na()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("pf_na_df", tidy_expr = list(new_col = is.na(x)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "logical"
  )
})

test_that("ds.mutate permits if_else()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = if_else(mpg > 20, 1, 0)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 4),
    round(mean(if_else(mtcars$mpg > 20, 1, 0)), 4)
  )
})

test_that("ds.mutate permits case_when()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = case_when(cyl == 4 ~ "small", cyl == 6 ~ "medium", cyl == 8 ~ "large")), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "character"
  )
})

# Other functions

test_that("ds.mutate permits desc()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = desc(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "numeric"
  )
})

# Nested function calls (regression test for c( regex)

test_that("ds.mutate permits as.numeric(as.Date()) nested call", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("pf_date_df", tidy_expr = list(new_col = as.numeric(as.Date(date_str))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("2024-01-15", "2024-06-30", "2024-12-25")))), 2)
  )
})
