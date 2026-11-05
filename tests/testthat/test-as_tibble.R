require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data("mtcars")

login_data <- .prepare_login(assign_method = "asTibbleDS")

conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", .test_table("mtcars"))
datashield.assign.table(conns, "cyl_2", .test_table("cyl_2"))
ds.bind_cols(to_combine = list(mtcars, cyl_2), .name_repair = "minimal", newobj = "mtcars_dup_names", datasources = conns)
datashield.assign.table(conns, "test_matrix_values", .test_table("test_matrix_values"))
ds.matrix(
  mdata = "test_matrix_values$value", from = "serverside.vector", nrows.scalar = 5, ncols.scalar = 4,
  newobj = "test_matrix", datasources = conns
)

test_that("ds.as_tibble correctly converts a data frame to a tibble", {
  skip_if_not_installed("dsBaseClient")
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
  skip_if_not_installed("dsBaseClient")
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
  skip_if_not_installed("dsBaseClient")
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
  skip_if_not_installed("dsBaseClient")
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
