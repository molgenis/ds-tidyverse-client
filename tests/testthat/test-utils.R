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
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

test_that(".get_datasources returns datasources found by datashield.connections_find() when datasources is NULL", {
  expect_equal(
    with_mocked_bindings(
      .get_datasources(datasources = NULL),
      "datashield.connections_find" = function() conns
    ),
    conns
  )
})

test_that(".get_datasources returns provided datasources when datasources is not NULL", {
  expect_equal(
    .get_datasources(datasources = conns),
    conns
  )
})

test_that(".verify_datasources throws an error if datasources contain non-DSConnection objects", {
  datasources <- c(conns, "not a DSConnection")
  expect_error(.verify_datasources(datasources), "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that(".verify_datasources does not throw an error if all datasources are DSConnection objects", {
  datasources <- conns
  expect_silent(.verify_datasources(datasources))
})

test_that(".set_datasources returns datasources found by datashield.connections_find() when datasources is NULL", {
  expect_equal(
    with_mocked_bindings(
      .set_datasources(datasources = NULL),
      "datashield.connections_find" = function() conns
    ),
    conns
  )
})

test_that(".set_datasources returns provided datasources when datasources is not NULL", {
  expect_equal(
    .set_datasources(datasources = conns),
    conns
  )
})

test_that(".set_datasources throws an error if datasources contain non-DSConnection objects", {
  datasources <- c(conns, "not a DSConnection")
  expect_error(.set_datasources(datasources), "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that(".set_datasources does not throw an error if all datasources are DSConnection objects", {
  datasources <- conns
  expect_silent(.set_datasources(datasources))
})

test_that(".set_new_obj returns new object name when provided", {
  newobj <- "my_new_object"
  result <- .set_new_obj(.data = "my_data", newobj = newobj)
  expect_equal(result, newobj)
})

test_that(".set_new_obj defaults to .data if no new object name is provided", {
  result <- .set_new_obj(.data = "my_data", newobj = NULL)
  expect_equal(result, "my_data")
})

disc_settings <- datashield.aggregate(conns, call("listDisclosureSettingsDS"))

test_that(".check_data_name_length throws an error if length of .data exceeds nfilter.string", {
  .data <- paste(rep("a", 101), collapse = "")
  expect_snapshot(.check_data_name_length(.data, disc_settings), error = TRUE)
})

test_that(".check_data_name_length does not throw an error if length of .data is within nfilter.string", {
  .data <- paste(rep("a", 79), collapse = "")
  expect_silent(.check_data_name_length(.data, disc_settings))
})

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n"),
    output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$",
               "$APO$", "$LSQ$", "$RSQ", "$EQU$", "$ADD$", "$SUB$", "$MULT$", "$DIVIDE$", "$POWER$",
               "$GT$", "$LT$", "$TILDE$", "$LINE$")
  )
  actual_encode_list <- .get_encode_dictionary()
  expect_equal(actual_encode_list, expected_encode_list)
})

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|", "=="),
    output = c("$LIST$", "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$EQUALS$")
  )
  actual_encode_list <- .get_encode_dictionary()
  expect_false(isTRUE(all.equal(expected_encode_list, actual_encode_list)))
})

test_that(".remove_list removes 'list(' portion from a string", {
  input_string <- "list(a, b, c)"
  output_string <- .remove_list(input_string)
  expect_equal(output_string, "a, b, c")
})

test_that(".remove_list handles input without 'list(' correctly", {
  input_string <- "c(a, b, c)"
  output_string <- .remove_list(input_string)
  expect_equal(output_string, "c(a, b, c")
})

test_that(".format_args_as_string correctly formats including 'list'", {
  expr <- rlang::expr(
    list(LAB_TSC, starts_with("LAB"), ends_with("ED"))
  )
  expected <- "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  result <- .format_args_as_string(expr)
  expect_equal(result, expected)
})

test_that(".format_args_as_string correctly formats excluding 'list'", {
  expr <- rlang::expr(
    c(LAB_TSC, starts_with("LAB"), ends_with("ED"))
  )
  expected <- "c(LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  result <- .format_args_as_string(expr)
  expect_equal(result, expected)
})

test_that(".encode_tidy_eval correctly encodes strings with permitted values", {
  encode_key <- .get_encode_dictionary()
  input_string <- "asd, qwe, starts_with('test')"
  expected_output <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$starts_with$LB$$APO$test$APO$$RB$"
  result <- .encode_tidy_eval(input_string, encode_key)
  expect_equal(result, expected_output)
})

test_that(".encode_tidy_eval correctly encodes strings with unpermitted values", {
  encode_key <- .get_encode_dictionary()
  input_string <- "asd, qwe, wer == rew &}{}%"
  expected_output <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$wer$SPACE$$EQU$$EQU$$SPACE$rew$SPACE$$AND$}{}%"
  result <- .encode_tidy_eval(input_string, encode_key)
  expect_equal(result, expected_output)
})

arg_permitted <- "asd, sdf, dfg, everything(), starts_with(\"A\"), ends_with(\"Z\")"
arg_unpermitted <- "asd, sdf, dfg, everything(), filter(test == 2), slice(3), mutate(new_name = old_name), starts_with(\"A\"), ends_with(\"Z\")"
small_var <- paste(rep("a", 5), collapse = "")
large_var <- paste(rep("a", 200), collapse = "")

test_that(".check_function_names allows permitted names to pass", {
  arg_permitted
  expect_silent(.check_function_names(arg_permitted))
})

test_that(".check_function_names blocks unpermitted function names", {
  arg_unpermitted
  expect_snapshot(
    .check_function_names(arg_unpermitted),
    error = TRUE
  )
})

test_that(".check_variable_length allows variables with value less than nfilter.string", {
  expect_silent(
    .check_variable_length(small_var, disc_settings, conns)
  )
})

test_that(".check_variable_length blocks variables with value greater than than nfilter.string", {
  expect_snapshot(
    .check_variable_length(large_var, disc_settings, conns),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks allows permitted arg to pass", {
  expect_silent(.check_tidy_disclosure("dataframe", arg_permitted, conns))
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted variable length", {
  arg_unpermitted_2 <- paste0(large_var, arg_permitted)
  expect_snapshot(
    .check_tidy_disclosure("dataframe", arg_unpermitted_2, conns),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted function names", {
  arg_unpermitted_3 <- arg_unpermitted
  expect_snapshot(
    .check_tidy_disclosure("dataset", arg_unpermitted_3, conns),
    error = TRUE
  )
})

test_that(".check_tidy_args returns correct errors", {
  expect_error(
    .check_tidy_args(df.name = NULL, newobj = "test"),
    "df.name is not a character vector"
  )

  expect_error(
    .check_tidy_args(df.name = "test", newobj = NULL),
    "newobj is not a character vector"
  )

  expect_silent(
    .check_tidy_args(df.name = "test", newobj = "test")
  )
})

test_that(".build_cally build correct call object", {
  input_string <- "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  expected <- call("mutateDS", "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")", "mydata")
  returned <- .build_cally("mutateDS", list(input_string, "mydata"))
  expect_equal(expected, returned)

  expected <- call("mutateDS", "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")", "mydata",
                   "extra_arg_1", "extra_arg_2")
  returned <- .build_cally("mutateDS", list(input_string, "mydata", "extra_arg_1", "extra_arg_2"))
  expect_equal(expected, returned)
})

test_that(".make_serverside_call encodes and builds call object", {
 input_string <- "c(LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
 tidy_select <- .format_args_as_string(rlang::enquo(input_string))
 returned <- .make_serverside_call("mutateDS", tidy_select, list("mydata"))
 expected <- call("mutateDS", "$QUOTE$c$LB$LAB_TSC$COMMA$$SPACE$starts_with$LB$\\$QUOTE$LAB\\$QUOTE$$RB$$COMMA$$SPACE$ends_with$LB$\\$QUOTE$ED\\$QUOTE$$RB$", "mydata")
 expect_equal(expected, returned)

 returned <- .make_serverside_call("mutateDS", tidy_select, list("mydata", "extra_arg_1", "extra_arg_2"))
 expected <- call("mutateDS", "$QUOTE$c$LB$LAB_TSC$COMMA$$SPACE$starts_with$LB$\\$QUOTE$LAB\\$QUOTE$$RB$$COMMA$$SPACE$ends_with$LB$\\$QUOTE$ED\\$QUOTE$$RB$",
                  "mydata", "extra_arg_1", "extra_arg_2")
 expect_equal(expected, returned)
})

test_that(".perform_tidyverse_checks checks argument validity and disclosure", {
  input_string <- "c(LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  tidy_select <- .format_args_as_string(rlang::enquo(input_string))
  expect_error(
    .perform_tidyverse_checks(NULL, "new_data", tidy_select, conns),
    "df.name is not a character vector"
  )

  expect_error(
    .perform_tidyverse_checks("my_data", NULL, tidy_select, conns),
    "newobj is not a character vector"
  )

  expect_silent(
    .perform_tidyverse_checks("my_data", "new_data", tidy_select, conns)
  )

  expect_error(
    .perform_tidyverse_checks(paste(rep("a", 101), collapse = ""), "new_data", tidy_select, conns),
    "Error: The length of string passed to"
  )

  arg_unpermitted <- "asd, sdf, dfg, everything(), filter(test == 2), slice(3), mutate(new_name = old_name), starts_with(\"A\"), ends_with(\"Z\")"
  tidy_select <- .format_args_as_string(rlang::enquo(input_string))
  expect_error(
    .perform_tidyverse_checks("my_data", "new_data", arg_unpermitted, conns),
    "tidy_select` must only contain Tidyverse select functions"
  )
})
