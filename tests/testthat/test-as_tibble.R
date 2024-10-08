library(DSLite)
library(DSI)
library(dplyr)
library(dsBase)
library(dsBaseClient)
library(dsTidyverse)

data("mtcars")
mtcars_dup_names <- cbind(mtcars, tibble(cyl = 2))

test_matrix <- matrix(data = 1:20, ncol = 4)

login_data <- .prepare_dslite(
  assign_method = "asTibbleDS",
  tables = list(
    mtcars = mtcars,
    mtcars_dup_names = mtcars_dup_names,
    test_matrix = test_matrix
  )
)

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "mtcars_dup_names", "mtcars_dup_names")
datashield.assign.table(conns, "test_matrix", "test_matrix")

test_that("ds.as_tibble correctly converts a data frame to a tibble", {
  ds.as_tibble(
    x = "mtcars",
    newobj = "mtcars_tib",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.dim("mtcars_tib", datasources = conns)[[1]],
    c(32, 11)
  )

  expect_equal(
    ds.colnames("mtcars_tib", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("ds.as_tibble works with the name_repair argument", {
  ds.as_tibble(
    x = "mtcars_dup_names",
    .name_repair = "minimal",
    newobj = "mtcars_tib_nr",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib_nr", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.colnames("mtcars_tib_nr", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl")
  )

  ds.as_tibble(
    x = "mtcars_dup_names",
    .name_repair = "unique",
    newobj = "mtcars_tib_nr",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib_nr", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.colnames("mtcars_tib_nr", datasources = conns)[[1]],
    c("mpg", "cyl...2", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl...12")
  )

  expect_error(
    ds.as_tibble(
      x = "mtcars_dup_names",
      .name_repair = "check_unique",
      newobj = "mtcars_tib_nr",
      datasources = conns
    ),
  )

  ds.as_tibble(
    x = "mtcars_dup_names",
    .name_repair = "universal",
    newobj = "mtcars_tib_nr",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib_nr", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.colnames("mtcars_tib_nr", datasources = conns)[[1]],
    c("mpg", "cyl...2", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl...12")
  )
})


test_that("ds.as_tibble works with the rownames argument", {
  ds.as_tibble(
    x = "mtcars",
    rownames = NULL,
    newobj = "mtcars_tib",
    datasources = conns
  ) ## Not really possible to test clientside as no datashield function to return row names.

  expect_equal(
    ds.class("mtcars_tib", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  ds.as_tibble(
    x = "mtcars",
    rownames = NA,
    newobj = "mtcars_tib",
    datasources = conns
  ) ## Same - can't test other than the object is created.

  expect_equal(
    ds.class("mtcars_tib", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  ds.as_tibble(
    x = "mtcars",
    rownames = "col_with_row_names",
    newobj = "mtcars_tib",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.colnames("mtcars_tib", datasources = conns)[[1]],
    c("col_with_row_names", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  expect_equal(
    ds.class("mtcars_tib$col_with_row_names", datasources = conns)[[1]],
    "character"
  )
})

test_that("ds.as_tibble works with matrices", {
  ds.as_tibble(
    x = "test_matrix",
    newobj = "mtcars_tib",
    .name_repair = "minimal",
    datasources = conns
  )

  expect_equal(
    ds.class("mtcars_tib", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    ds.colnames("mtcars_tib", datasources = conns)[[1]],
    c("", "", "", "")
  )
})
