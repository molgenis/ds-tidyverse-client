require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

data("mtcars")
login_data <- .prepare_dslite()
conns <- datashield.login(logins = login_data)

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

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", "!", "&", "|", "'", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n", "%"),
    output = c(
      "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$EXCL$", "$AND$", "$OR$",
      "$APO$", "$EQU$", "$ADD$", "$SUB$", "$MULT$", "$DIVIDE$", "$POWER$",
      "$GT$", "$LT$", "$TILDE$", "$LINE$", "$PCT$"
    )
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

test_that(".format_args_as_string removes the outer 'list()'", {
  quo <- rlang::quo(list(LAB_TSC, starts_with("LAB"), ends_with("ED")))
  expected <- "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  expect_equal(.format_args_as_string(quo, "tidy_expr"), expected)
})

test_that(".format_args_as_string leaves an inner 'list(' intact", {
  quo <- rlang::quo(list(x = enlist(a)))
  expect_equal(.format_args_as_string(quo, "tidy_expr"), "x = enlist(a)")
})

test_that(".format_args_as_string returns NULL when no expression is passed", {
  expect_null(.format_args_as_string(rlang::quo(NULL), "tidy_expr"))
})

test_that(".format_args_as_string throws an error if the expression is not wrapped in 'list()'", {
  expected <- "`condition` must be wrapped in `list\\(\\)`"
  expect_error(.format_args_as_string(rlang::quo(mpg > 20), "condition"), expected)
  expect_error(.format_args_as_string(rlang::quo(desc(mpg)), "condition"), expected)
  expect_error(.format_args_as_string(rlang::quo(cyl), "condition"), expected)
  expect_error(.format_args_as_string(rlang::quo(c(mpg, cyl)), "condition"), expected)
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
  expected_output <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$wer$SPACE$$EQU$$EQU$$SPACE$rew$SPACE$$AND$}{}$PCT$"
  result <- .encode_tidy_eval(input_string, encode_key)
  expect_equal(result, expected_output)
})

test_that(".build_cally build correct call object", {
  input_string <- "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")"
  expected <- call("mutateDS", "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")", "mydata")
  returned <- .build_cally("mutateDS", list(input_string, "mydata"))
  expect_equal(expected, returned)

  expected <- call(
    "mutateDS", "LAB_TSC, starts_with(\"LAB\"), ends_with(\"ED\")", "mydata",
    "extra_arg_1", "extra_arg_2"
  )
  returned <- .build_cally("mutateDS", list(input_string, "mydata", "extra_arg_1", "extra_arg_2"))
  expect_equal(expected, returned)
})

test_that(".make_serverside_call encodes and builds call object", {
  tidy_select <- .format_args_as_string(
    rlang::quo(list(c(LAB_TSC, starts_with("LAB"), ends_with("ED")))), "tidy_expr"
  )
  returned <- .make_serverside_call("mutateDS", tidy_select, list("mydata"))
  expected <- call("mutateDS", "c$LB$LAB_TSC$COMMA$$SPACE$starts_with$LB$$QUOTE$LAB$QUOTE$$RB$$COMMA$$SPACE$ends_with$LB$$QUOTE$ED$QUOTE$$RB$$RB$", "mydata")
  expect_equal(expected, returned)

  returned <- .make_serverside_call("mutateDS", tidy_select, list("mydata", "extra_arg_1", "extra_arg_2"))
  expected <- call(
    "mutateDS", "c$LB$LAB_TSC$COMMA$$SPACE$starts_with$LB$$QUOTE$LAB$QUOTE$$RB$$COMMA$$SPACE$ends_with$LB$$QUOTE$ED$QUOTE$$RB$$RB$",
    "mydata", "extra_arg_1", "extra_arg_2"
  )
  expect_equal(expected, returned)
})

test_that(".check_tidy_args returns correct errors", {
  expect_error(
    .check_tidy_args(df.name = NULL, newobj = "test", check_df = TRUE, check_obj = TRUE),
    "df.name is not a character vector"
  )

  expect_error(
    .check_tidy_args(df.name = "test", newobj = NULL, check_df = TRUE, check_obj = TRUE),
    "`newobj` must be a single non-empty character string"
  )

  for (bad_newobj in list(1, c("a", "b"), NA_character_, "")) {
    expect_error(
      .check_tidy_args(df.name = "test", newobj = bad_newobj),
      "`newobj` must be a single non-empty character string"
    )
  }

  expect_silent(
    .check_tidy_args(df.name = "test", newobj = "test", check_df = TRUE, check_obj = TRUE)
  )
})
