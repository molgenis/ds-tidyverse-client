library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

data(mtcars)
login_data <- .prepare_dslite(assign_method = "bindRowsDS", tables = list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("ds.rename fails with correct error message if data not present ", {
  expect_error(
    ds.rename(
      df.name = "datanotthere",
      tidy_select = list(test_1 = mpg, test_2 = drat),
      newobj = "nodata",
      datasources = conns
    )
  )
})

test_that("ds.rename correctly passes =", {
  ds.rename(
    df.name = "mtcars",
    tidy_select = list(test_1 = mpg, test_2 = drat),
    newobj = "mpg_drat",
    datasources = conns
  )

  expect_equal(
    ds.colnames("mpg_drat", datasources = conns)[[1]],
    c("test_1", "cyl", "disp", "hp", "test_2", "wt", "qsec", "vs", "am", "gear", "carb")
  )

})

test_that("ds.rename throws an error if column name doesn't exist", {
  expect_error(
    ds.rename(
      df.name = "mtcars",
      tidy_select = list(test_1 = doesntexist, test_2 = drat),
      newobj = "mpg_drat",
      datasources = conns
    )
  )
})
