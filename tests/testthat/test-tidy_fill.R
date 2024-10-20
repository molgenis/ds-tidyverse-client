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
dslite.server$aggregateMethod("getAllLevelsDS", "getAllLevelsDS")
dslite.server$assignMethod("setAllLevelsDS", "setAllLevelsDS")

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

####################################################################################################
# Code that will be used in multiple tests
####################################################################################################
var_class <- .get_var_classes("df", datasources = conns)

class_conflicts <- .identify_class_conflicts(var_class)

different_classes <- c("fac_col4", "fac_col5")

class_decisions <- c("1", "5")

.fix_classes(
  df.name = "df",
  different_classes = different_classes,
  class_decisions = class_decisions,
  newobj = "new_classes",
  datasources = conns)

cols_to_set <- c(
  "fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col9", "col12",
  "col15", "col18", "fac_col7", "fac_col10", "col13", "col16", "col19", "col11", "col14", "col17",
  "col20")

.add_missing_cols_to_df(
  df.name = "df",
  cols_to_add_if_missing = cols_to_set,
  newobj = "with_new_cols",
  datasources = conns)

old_cols <- ds.colnames("df")
new_cols <- c("col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
              "col20", "fac_col1", "fac_col10", "fac_col2", "fac_col3", "fac_col4", "fac_col5",
              "fac_col6", "fac_col7", "fac_col9")

added_cols <- .get_added_cols(old_cols, new_cols_servers)

var_class_fact <- .get_var_classes("with_new_cols", datasources = conns)

fac_vars <- .identify_factor_vars(var_class_fact)

fac_levels <- .get_factor_levels(fac_vars, "with_new_cols", conns)

unique_levs <- .get_unique_levels(fac_levels, level_conflicts)

####################################################################################################
# Tests
####################################################################################################
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

test_that("ask_question_wait_response_class repeats with invalid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_class("a variable"),
      ask_question = function() "A question",
      readline = function() "9"
    ), "Invalid input. Please try again"
  )
})

# print_all_classes
# prompt_user_class_decision
# prompt_user_class_decision_all_vars

test_that(".fix_classes sets the correct classes in serverside data frame", {

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

  new_cols_servers <- list(
    server_1 = new_cols,
    server_2 = new_cols,
    server_3 = new_cols
  )

  expect_equal(
    added_cols,
    list(
      server_1 = c("col11", "col13", "col14", "col16", "col17", "col19", "col20", "fac_col10", "fac_col7"),
      server_2 = c("col11", "col12", "col14", "col15", "col17", "col18", "col20", "fac_col6", "fac_col9"),
      server_3 = c("col12", "col13", "col15", "col16", "col18", "col19", "fac_col10", "fac_col6", "fac_col7", "fac_col9")
    )
  )
})

test_that(".identify_factor_vars correctly identifies factor variables", {
  var_class_fact <- var_class |> dplyr::select(server: col18)
  expect_equal(
    names(fac_vars),
    c("fac_col1", "fac_col2", "fac_col3", "fac_col6", "fac_col9")
    )
})

test_that(".get_factor_levels correctly identifies factor levels", {
  expected <- list(
    server_1 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue", "Green"),
      fac_col3 = c("No", "Yes"),
      fac_col6 = c("Bird", "Cat", "Dog"),
      fac_col9 = c("False", "True")
    ),
    server_2 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Green", "Red"),
      fac_col3 = c("No"),
      fac_col6 = NULL,
      fac_col9 = NULL
    ),
    server_3 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue"),
      fac_col3 = c("Yes"),
      fac_col6 = NULL,
      fac_col9 = NULL
    )
  )

  expect_equal(fac_levels, expected)
})

level_conflicts <- .identify_level_conflicts(fac_levels)

test_that(".identify_level_conflicts correctly factor columns with different levels", {
  expect_equal(
    .identify_level_conflicts(fac_levels),
    c("fac_col2", "fac_col3", "fac_col6", "fac_col9")
  )

})

test_that("ask_question_wait_response_levels continues with valid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_levels("a variable"),
      ask_question = function() "A question",
      readline = function() "1"
    ), "1"
  )
})

test_that("ask_question_wait_response_levels repeats with invalid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_levels("a variable"),
      ask_question = function() "A question",
      readline = function() "9"
    ), "Invalid input. Please try again"
  )
})

test_that(".make_levels_message makes correct message", {
  expect_snapshot(.make_levels_message(level_conflicts))
})

test_that(".get_unique_levels extracts all possible levels", {

  expected <- list(
    fac_col2 = c("Blue", "Green", "Red"),
    fac_col3 = c("No", "Yes"),
    fac_col6 = c("Bird", "Cat", "Dog"),
    fac_col9 = c("False", "True")
  )

  expect_equal(unique_levs, expected)

})

test_that(".set_factor_levels sets levels correctly", {
  .set_factor_levels("with_new_cols", unique_levs,  conns)

  expect_equal(
    ds.levels("with_new_cols$fac_col2") |> map(~.x[[1]]),
    list(
      server_1 = c("Blue", "Green", "Red"),
      server_2 = c("Blue", "Green", "Red"),
      server_3 = c("Blue", "Green", "Red")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col3") |> map(~.x[[1]]),
    list(
      server_1 = c("No", "Yes"),
      server_2 = c("No", "Yes"),
      server_3 = c("No", "Yes")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col6") |> map(~.x[[1]]),
    list(
      server_1 = c("Bird", "Cat", "Dog"),
      server_2 = c("Bird", "Cat", "Dog"),
      server_3 = c("Bird", "Cat", "Dog")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col9") |> map(~.x[[1]]),
    list(
      server_1 = c("False", "True"),
      server_2 = c("False", "True"),
      server_3 = c("False", "True")
    )
  )

})

test_that(".print_var_recode_message prints the correct message", {
  expect_snapshot(.print_var_recode_message(added_cols, "test_df"))
})

test_that(".print_class_recode_message prints the correct message", {
  expect_snapshot(
    .print_class_recode_message(class_decisions, different_classes, "test_df")
    )
})

test_that(".print_levels_recode_message prints the correct message", {
  expect_snapshot(
    .print_levels_recode_message(unique_levs, "test_df")
  )
})

test_that(".make_levels_recode_message prints the correct message", {
  expect_equal(
    .make_levels_recode_message(unique_levs),
    list(
      "fac_col2 --> Blue, Green, Red",
      "fac_col3 --> No, Yes",
      "fac_col6 --> Bird, Cat, Dog",
      "fac_col9 --> False, True"
    )
  )
})

test_that(".print_out_messages prints the correct messages", {
  expect_snapshot(
    .print_out_messages(
      added_cols, class_decisions, different_classes, unique_levs, level_conflicts, "1", "test_df"
      )
  )
})

test_that(".change_choice_to_string converts numeric class codes to strings correctly", {
  expect_equal(.change_choice_to_string("1"), "factor")
  expect_equal(.change_choice_to_string("2"), "integer")
  expect_equal(.change_choice_to_string("3"), "numeric")
  expect_equal(.change_choice_to_string("4"), "character")
  expect_equal(.change_choice_to_string("5"), "logical")
})

# Diferentiate new and old objects so these can plausibly be removed
# Improve error messages for levels and class so you can see change in each cohort


