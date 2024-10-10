library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(purrr)

df <- create_mixed_dataframe(n_rows = 100, n_factor_cols = 10, n_other_cols = 10)

df_1 <- df %>% select(1:5, 6, 9, 12, 15, 18) %>%
  mutate(
    fac_col2 = factor(fac_col2, levels = c("Blue", "Green")),
    fac_col4 = as.numeric(fac_col4),
    fac_col5 = as.logical(fac_col5))

df_2 <- df %>% select(1:5, 7, 10, 13, 16, 19) %>%
  mutate(
    fac_col2 = factor(fac_col2, levels = c("Green", "Red")),
    fac_col3 = factor(fac_col3, levels = "No"),
    fac_col4 = as.character(fac_col4),
    fac_col5 = as.integer(fac_col5))

df_3 <- df %>% select(1:5, 11, 14, 17, 20) %>%
  mutate(
    fac_col2 = factor(fac_col2, levels = "Blue"),
    fac_col3 = factor(fac_col3, levels = "Yes"))

options(datashield.env = environment())

dslite.server <- newDSLiteServer(
  tables = list(
    df_1 = df_1,
    df_2 = df_2,
    df_3 = df_3
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$aggregateMethod("classAllColsDS", "classAllColsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server = "server_1",
  url = "dslite.server",
  driver = "DSLiteDriver"
)

builder$append(
  server = "server_2",
  url = "dslite.server",
  driver = "DSLiteDriver"
)

builder$append(
  server = "server_3",
  url = "dslite.server",
  driver = "DSLiteDriver"
)

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = FALSE)

datashield.assign.table(conns["server_1"], "df", "df_1")
datashield.assign.table(conns["server_2"], "df", "df_2")
datashield.assign.table(conns["server_3"], "df", "df_3")

test_that(".stop_if_cols_identical throws error if columns are identical", {

  identical_cols <- list(
    c("col1", "col2", "col3"),
    c("col1", "col2", "col3"),
    c("col1", "col2", "col3")
  )

  expect_error(
    .stop_if_cols_identical(identical_cols),
    "Columns are identical in all data frames: nothing to fill"
  )

})

test_that(".stop_if_cols_identical doesn't throw error if data frames have different columns", {

  different_cols <- list(
    c("col1", "col2", "col3"),
    c("col1", "col2", "col4"),
    c("col1", "col5", "col3")
  )

  expect_silent(
    .stop_if_cols_identical(different_cols)
  )

})

var_class <- .get_var_classes("df", datasources = conns)

test_that(".get_var_classes returns correct output", {

  expected <- tibble(
    server = c("server_1", "server_2", "server_3"),
    fac_col1 = c("factor", "factor", "factor"),
    fac_col2 = c("factor", "factor", "factor"),
    fac_col3 = c("factor", "factor", "factor"),
    fac_col4 = c("numeric", "character", "factor"),
    fac_col5 = c("logical", "integer", "factor"),
    fac_col6 = c("factor", NA, NA),
    fac_col9 = c("factor", NA, NA),
    col12 = c("numeric", NA, NA),
    col15 = c("integer", NA, NA),
    col18 = c("logical", NA, NA),
    fac_col7 = c(NA, "factor", NA),
    fac_col10 = c(NA, "factor", NA),
    col13 = c(NA, "character", NA),
    col16 = c(NA, "numeric", NA),
    col19 = c(NA, "integer", NA),
    col11 = c(NA, NA, "integer"),
    col14 = c(NA, NA, "logical"),
    col17 = c(NA, NA, "character"),
    col20 = c(NA, NA, "numeric")
  )

  expect_equal(var_class, expected)

})

class_conflicts <- .identify_class_conflicts(var_classes)

test_that(".identify_class_conflicts returns correct output", {
  expected <- list(
    fac_col4 = c("numeric", "character", "factor"),
    fac_col5 = c("logical", "integer", "factor")
  )

  expect_equal(class_conflicts, expected)

})

test_that("ask_question displays the correct prompt", {
  expect_snapshot(ask_question("my_var"))
})

