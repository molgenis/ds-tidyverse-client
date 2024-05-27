library(DSLite)
library(dplyr)
library(dsBaseClient)
library(testthat)
library(purrr)
library(cli)
library(dsTidyverse)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "dsTidyverse::selectDS")
dslite.server$assignMethod("renameDS", "dsTidyverse::renameDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

.check_cols_as_expected <- function(expected, df) {
  observed <- ds.colnames(df)[[1]]
  expected <- expected
  expect_equal(observed, expected)
}

test_that("ds.rename correctly renames where columns exist : ", {
  ds.rename(
    .data = "mtcars",
    tidy_select = list(new_name_1 = mpg, new_name_2 = drat),
    newobj = "mtcars_renamed"
  )
  expected <- c("new_name_1", "cyl", "disp", "hp", "new_name_2", "wt", "qsec", "vs", "am", "gear", "carb")
  .check_cols_as_expected(expected, "mtcars_renamed")
})

# test_that("ds.rename failes if the target column doesn't exist exist : ", {
#   expect_snapshot(
#     ds.rename(
#       .data = "mtcars",
#       tidy_select = list(test_col_1 = doesnt_exist, test_col_2 = drat),
#       newobj = "mtcars_renamed"
#     )
#   )
#   })## Wait until DSI updated

