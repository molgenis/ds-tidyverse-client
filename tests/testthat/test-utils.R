logindata.dslite.cnsim <- setupCNSIMTest()
conns <- datashield.login(logindata.dslite.cnsim, assign = TRUE)

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

test_that(".check_data_name_length throws an error if length of .data exceeds nfilter.string", {
  .data <- paste(rep("a", 101), collapse = "")
  nfilter.string <- 100
  expect_snapshot(.check_data_name_length(.data, nfilter.string), error = TRUE)
})

test_that(".check_data_name_length does not throw an error if length of .data is within nfilter.string", {
  .data <- paste(rep("a", 99), collapse = "")
  nfilter.string <- 100
  expect_silent(.check_data_name_length(.data, nfilter.string))
})

test_that(".getEncodeKey returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "="),
    output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$APO$", "$LSQ$", "$RSQ", "$EQU$")
  )
  actual_encode_list <- .getEncodeKey()
  expect_equal(actual_encode_list, expected_encode_list)
})

test_that(".getEncodeKey returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|", "=="),
    output = c("$LIST$", "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$EQUALS$")
  )
  actual_encode_list <- .getEncodeKey()
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
  encode_key <- .getEncodeKey()
  input_string <- "asd, qwe, starts_with('test')"
  expected_output <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$starts_with$LB$$APO$test$APO$$RB$"
  result <- .encode_tidy_eval(input_string, encode_key)
  expect_equal(result, expected_output)
})

test_that(".encode_tidy_eval correctly encodes strings with unpermitted values", {
  encode_key <- .getEncodeKey()
  input_string <- "asd, qwe, wer == rew ^}{}/&%"
  expected_output <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$wer$SPACE$$EQU$$EQU$$SPACE$rew$SPACE$^}{}/$AND$%"
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
    .check_variable_length(small_var, 100)
  )
})

test_that(".check_variable_length blocks variables with value greater than than nfilter.string", {
  expect_snapshot(
    .check_variable_length(large_var, 100),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks allows permitted arg to pass", {
  expect_silent(.tidy_disclosure_checks(arg_permitted, nfilter.string = 100))
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted variable length", {
  arg_unpermitted_2 <- paste0(large_var, arg_permitted)
  expect_snapshot(
    .tidy_disclosure_checks(arg_unpermitted_2, nfilter.string = 100),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted function names", {
  arg_unpermitted_3 <- arg_unpermitted
  expect_snapshot(
    .tidy_disclosure_checks(arg_unpermitted_3, nfilter.string = 100),
    error = TRUE
  )
})
