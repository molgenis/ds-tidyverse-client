library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "dsTidyverse::selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

.check_cols_as_expected <- function(expected, df) {
  observed <- ds.colnames(df)[[1]]
  expected <- expected
  expect_equal(observed, expected)
}

# test_that("ds.select fails with correct error message if data not present ", {
#   expect_snapshot(
#     ds.select(
#       df.name = "datanotthere",
#       tidy_select = list(mpg:drat),
#       newobj = "nodata"))
#   })## Wait until DSI updated

test_that("ds.select correctly passes : ", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(mpg:drat),
    newobj = "mpg_drat"
  )
  expected <- c("mpg", "cyl", "disp", "hp", "drat")
  .check_cols_as_expected(expected, "mpg_drat")
})

test_that("ds.select correctly passes `starts_with`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("m")),
    newobj = "starts"
  )
  expected <- "mpg"
  .check_cols_as_expected(expected, "starts")
})

test_that("ds.select correctly passes `ends_with`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(ends_with("m")),
    newobj = "ends"
  )
  expected <- "am"
  .check_cols_as_expected(expected, "ends")
})

test_that("ds.select correctly passes `matches`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(matches("[aeiou]")),
    newobj = "matches"
  )
  expected <- c("disp", "drat", "qsec", "am", "gear", "carb")
  .check_cols_as_expected(expected, "matches")
})

test_that("ds.select correctly passes `everything`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(everything()),
    newobj = "everything"
  )
  expected <- colnames(mtcars)
  .check_cols_as_expected(expected, "everything")
})

test_that("ds.select correctly passes `last_col`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(last_col()),
    newobj = "last"
  )
  expected <- "carb"
  .check_cols_as_expected(expected, "last")
})

test_that("ds.select correctly passes `group_cols`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(group_cols()),
    newobj = "group"
  )
  expected <- character(0)
  .check_cols_as_expected(expected, "group")
})

test_that("ds.select correctly passes strings with '&'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("c") & ends_with("b")),
    newobj = "and"
  )
  expected <- "carb"
  .check_cols_as_expected(expected, "and")
})

test_that("ds.select correctly passes strings with '!'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(!mpg),
    newobj = "not"
  )
  expected <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  .check_cols_as_expected(expected, "not")
})

test_that("ds.select correctly passes strings with '|'", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(starts_with("c") | ends_with("b")),
    newobj = "or"
  )
  expected <- c("cyl", "carb")
  .check_cols_as_expected(expected, "or")
})

test_that("ds.select correctly passes `strings with `all_of`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(all_of(c("mpg", "cyl"))),
    newobj = "all_of"
  )
  expected <- c("mpg", "cyl")
  .check_cols_as_expected(expected, "all_of")
})

test_that("ds.select correctly passes strings with `any_of`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(any_of(c("mpg", "cyl"))),
    newobj = "any_of"
  )
  expected <- c("mpg", "cyl")
  .check_cols_as_expected(expected, "any_of")
})

test_that("ds.select correctly passes complex strings", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list((starts_with("c") & ends_with("b")) | contains("ra") | gear:carb),
    newobj = "complex"
  )
  expected <- c("carb", "drat", "gear")
  .check_cols_as_expected(expected, "complex")
})

test_that("ds.select correctly passes strings with `where`", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(where(is.numeric)),
    newobj = "where"
  )
  expected <- colnames(mtcars)
  .check_cols_as_expected(expected, "where")
})

test_that("ds.select correctly passes strings with '='", {
  ds.select(
    df.name = "mtcars",
    tidy_select = list(test = mpg, cyl, gear),
    newobj = "equals"
  )
  expected <- c("test", "cyl", "gear")
  .check_cols_as_expected(expected, "equals")
})
