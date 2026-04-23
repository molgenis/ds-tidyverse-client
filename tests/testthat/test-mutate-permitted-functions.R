require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data(mtcars)
na_df <- data.frame(x = c(1, NA, 3, NA, 5), stringsAsFactors = FALSE)
date_df <- data.frame(date_str = c("2024-01-15", "2024-06-30", "2024-12-25"), stringsAsFactors = FALSE)
date_dmy_df <- data.frame(date_str = c("15/01/2024", "30/06/2024", "25/12/2024"), stringsAsFactors = FALSE)
date_mdy_df <- data.frame(date_str = c("01/15/2024", "06/30/2024", "12/25/2024"), stringsAsFactors = FALSE)
date_dmy_dash_df <- data.frame(date_str = c("15-01-2024", "30-06-2024", "25-12-2024"), stringsAsFactors = FALSE)
date_ymd_slash_df <- data.frame(date_str = c("2024/01/15", "2024/06/30", "2024/12/25"), stringsAsFactors = FALSE)
date_dby_df <- data.frame(date_str = c("15 Jan 2024", "30 Jun 2024", "25 Dec 2024"), stringsAsFactors = FALSE)
date_bdy_df <- data.frame(date_str = c("Jan 15, 2024", "Jun 30, 2024", "Dec 25, 2024"), stringsAsFactors = FALSE)
date_compact_df <- data.frame(date_str = c("20240115", "20240630", "20241225"), stringsAsFactors = FALSE)
date_dot_df <- data.frame(date_str = c("15.01.2024", "30.06.2024", "25.12.2024"), stringsAsFactors = FALSE)
login_data <- .prepare_dslite(
  assign_method = "mutateDS",
  tables = list(
    mtcars = mtcars, na_df = na_df, date_df = date_df,
    date_dmy_df = date_dmy_df, date_mdy_df = date_mdy_df,
    date_dmy_dash_df = date_dmy_dash_df, date_ymd_slash_df = date_ymd_slash_df,
    date_dby_df = date_dby_df, date_bdy_df = date_bdy_df,
    date_compact_df = date_compact_df, date_dot_df = date_dot_df
  )
)
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "na_df", "na_df")
datashield.assign.table(conns, "date_df", "date_df")
datashield.assign.table(conns, "date_dmy_df", "date_dmy_df")
datashield.assign.table(conns, "date_mdy_df", "date_mdy_df")
datashield.assign.table(conns, "date_dmy_dash_df", "date_dmy_dash_df")
datashield.assign.table(conns, "date_ymd_slash_df", "date_ymd_slash_df")
datashield.assign.table(conns, "date_dby_df", "date_dby_df")
datashield.assign.table(conns, "date_bdy_df", "date_bdy_df")
datashield.assign.table(conns, "date_compact_df", "date_compact_df")
datashield.assign.table(conns, "date_dot_df", "date_dot_df")

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
  ds.mutate("date_df", tidy_expr = list(new_col = as.Date(date_str)), newobj = "res", datasources = conns)
  expect_equal(
    ds.class("res$new_col", datasources = conns)[[1]],
    "Date"
  )
})

test_that("ds.mutate permits as.Date() with positional format argument", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_dmy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%d/%m/%Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("15/01/2024", "30/06/2024", "25/12/2024"), "%d/%m/%Y"))), 2)
  )
})

test_that("ds.mutate as.Date() without format gives wrong result for non-ISO dates", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_dmy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str))), newobj = "res_wrong", datasources = conns)
  correct_mean <- round(mean(as.numeric(as.Date(c("15/01/2024", "30/06/2024", "25/12/2024"), "%d/%m/%Y"))), 2)
  wrong_mean <- round(ds.mean("res_wrong$date_num", datasources = conns)$Mean.by.Study[1, 1], 2)
  expect_false(wrong_mean == correct_mean)
})

test_that("ds.mutate permits as.Date() with US format %m/%d/%Y", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_mdy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%m/%d/%Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("01/15/2024", "06/30/2024", "12/25/2024"), "%m/%d/%Y"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with dash format %d-%m-%Y", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_dmy_dash_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%d-%m-%Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("15-01-2024", "30-06-2024", "25-12-2024"), "%d-%m-%Y"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with %Y/%m/%d format", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_ymd_slash_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%Y/%m/%d"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("2024/01/15", "2024/06/30", "2024/12/25"), "%Y/%m/%d"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with abbreviated month %d %b %Y", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_dby_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%d %b %Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("15 Jan 2024", "30 Jun 2024", "25 Dec 2024"), "%d %b %Y"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with %b %d, %Y format", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_bdy_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%b %d, %Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("Jan 15, 2024", "Jun 30, 2024", "Dec 25, 2024"), "%b %d, %Y"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with compact %Y%m%d format", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_compact_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%Y%m%d"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("20240115", "20240630", "20241225"), "%Y%m%d"))), 2)
  )
})

test_that("ds.mutate permits as.Date() with dot format %d.%m.%Y", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_dot_df", tidy_expr = list(date_num = as.numeric(as.Date(date_str, "%d.%m.%Y"))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$date_num", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("15.01.2024", "30.06.2024", "25.12.2024"), "%d.%m.%Y"))), 2)
  )
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
  ds.mutate("na_df", tidy_expr = list(new_col = is.na(x)), newobj = "res", datasources = conns)
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

test_that("ds.mutate permits scale()", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("mtcars", tidy_expr = list(new_col = scale(mpg)), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(scale(mtcars$mpg)), 2)
  )
})

# Nested function calls (regression test for c( regex)

test_that("ds.mutate permits as.numeric(as.Date()) nested call", {
  skip_if_not_installed("dsBaseClient")
  ds.mutate("date_df", tidy_expr = list(new_col = as.numeric(as.Date(date_str))), newobj = "res", datasources = conns)
  expect_equal(
    round(ds.mean("res$new_col", datasources = conns)$Mean.by.Study[1, 1], 2),
    round(mean(as.numeric(as.Date(c("2024-01-15", "2024-06-30", "2024-12-25")))), 2)
  )
})
