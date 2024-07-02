library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("renameDS", "dsTidyverse::renameDS")
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

test_that("ds.rename fails with correct error message if data not present ", {
    expect_error(
      ds.rename(
        df.name = "datanotthere",
        tidy_select = list(test_1 = mpg, test_2 = drat),
        newobj = "nodata")
    )
})

test_that("ds.rename correctly passes =", {
    ds.rename(
      df.name = "mtcars",
      tidy_select = list(test_1 = mpg, test_2 = drat),
      newobj = "mpg_drat"
  )
  expected <- c("test_1", "cyl", "disp", "hp", "test_2", "wt", "qsec", "vs", "am", "gear", "carb")
  .check_cols_as_expected(expected, "mpg_drat")
})

test_that("ds.rename throws an error if column name doesn't exist", {
  expect_error(
    ds.rename(
      df.name = "mtcars",
      tidy_select = list(test_1 = doesntexist, test_2 = drat),
      newobj = "mpg_drat"
    )
  )
})
