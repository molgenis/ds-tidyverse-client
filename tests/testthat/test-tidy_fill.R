library(DSLite)
library(dplyr)
library(dsBase)
library(dsBaseClient)
library(purrr)
# devtools::load_all("~/Library/Mobile Documents/com~apple~CloudDocs/work/repos/dsTidyverse")

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
dslite.server$assignMethod("fixClassDS", "fixClassDS")
dslite.server$assignMethod("makeColsSameDS", "makeColsSameDS")

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

class_conflicts <- .identify_class_conflicts(var_class)

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

test_that("check_response_class aborts on input '6'", {
  expect_error(check_response_class("6", "variable_name"), "Aborted `ds.dataFrameFill`")
})

test_that("check_response_class returns valid input", {
  expect_equal(check_response_class("1", "variable_name"), "1")
  expect_equal(check_response_class("2", "variable_name"), "2")
  expect_equal(check_response_class("3", "variable_name"), "3")
  expect_equal(check_response_class("4", "variable_name"), "4")
  expect_equal(check_response_class("5", "variable_name"), "5")
})

test_that("check_response_class warns on invalid input", {
  expect_message(
    check_response_class("invalid", "variable_name"),
    "Invalid input. Please try again."
  )
})

test_that("check_response_class calls question() on invalid input", {
  expect_equal(
    with_mocked_bindings(
      check_response_class("invalid", "variable_name"),
      ask_question = function(var) "prompt"
    ),
    "prompt"
  )
})

test_that("ask_question_wait_response_class continues with valid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_class("a variable"),
      ask_question = function() "A question",
      readline = function() "1"
    ), "1"
  )
})

test_that("ask_question_wait_response_class stinues with valid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_class("a variable"),
      ask_question = function() "A question",
      readline = function() "1"
    ), "1"
  )
})
#
# prompt_user_class_decision
# prompt_user_class_decision_all_vars

test_that(".fix_classes sets the correct classes in serverside data frame", {
.fix_classes(
  df.name = "df",
  different_classes = c("fac_col4", "fac_col5"),
  class_decisions = c("1", "5"),
  newobj = "new_classes",
  datasources = conns)

expect_equal(
  unname(unlist(ds.class("df$fac_col4"))),
  c("numeric", "character", "factor")
)

expect_equal(
  unname(unlist(ds.class("df$fac_col5"))),
  c("logical", "integer", "factor")
)

expect_equal(
  unname(unlist(ds.class("new_classes$fac_col4"))),
  rep("factor", 3)
)

expect_equal(
  unname(unlist(ds.class("new_classes$fac_col5"))),
  rep("logical", 3)
)

})

test_that(".get_unique_cols extracts unique names from a list", {
  expect_equal(
    .get_unique_cols(
      list(
        server_1 = c("col_1", "col_2", "col_3"),
        server_1 = c("col_1", "col_2", "col_4"),
        server_1 = c("col_2", "col_3", "col_3", "col_5")
        )
      ),
    c("col_1", "col_2", "col_3", "col_4", "col_5")
  )
})

test_that(".add_missing_cols_to_df correctly creates missing columns", {
  cols_to_set <- c(
    "fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col9", "col12",
    "col15", "col18", "fac_col7", "fac_col10", "col13", "col16", "col19", "col11", "col14", "col17",
    "col20")

  .add_missing_cols_to_df(
    df.name = "df",
    cols_to_add_if_missing = cols_to_set,
    newobj = "with_new_cols",
    datasources = conns)

  new_cols <- c("col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
                "col20", "fac_col1", "fac_col10", "fac_col2", "fac_col3", "fac_col4", "fac_col5",
                "fac_col6", "fac_col7", "fac_col9")

  observed <- ds.colnames("with_new_cols")

  expected <- list(
    server_1 = new_cols,
    server_2 = new_cols,
    server_3 = new_cols
  )

  expect_equal(observed, expected)
})

test_that(".get_added_cols correctly identifies newly added columns", {

  old_cols <- ds.colnames("df")
  new_cols <- c("col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
                "col20", "fac_col1", "fac_col10", "fac_col2", "fac_col3", "fac_col4", "fac_col5",
                "fac_col6", "fac_col7", "fac_col9")

  new_cols_servers <- list(
    server_1 = new_cols,
    server_2 = new_cols,
    server_3 = new_cols
  )

  .get_added_cols(old_cols, new_cols_servers)


# .identify_factor_vars
# .get_factor_levels
# .identify_level_conflicts
# ask_question_wait_response_levels
# .make_levels_message
# check_response_levels
# .get_unique_levels
# .set_factor_levels
# .print_out_messages
# .print_var_recode_message
# .print_class_recode_message
# .print_levels_recode_message
# .make_levels_recode_message
# change_choice_to_string
# print_all_classes
