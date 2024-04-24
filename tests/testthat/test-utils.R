logindata.dslite.cnsim <- setupCNSIMTest()
conns <- datashield.login(logindata.dslite.cnsim, assign=TRUE)

test_that(".get_datasources returns datasources found by datashield.connections_find() when datasources is NULL", {
  expect_equal(
    with_mocked_bindings(
      .get_datasources(datasources = NULL),
      "datashield.connections_find" = function() conns
      ),
    conns)
})

test_that(".get_datasources returns provided datasources when datasources is not NULL", {
  expect_equal(
    .get_datasources(datasources = conns),
    conns)
})

test_that(".verify_datasources throws an error if datasources contain non-DSConnection objects", {
  datasources <- list(DSConnection$new(), DSConnection$new(), "not a DSConnection")
  expect_error(.verify_datasources(datasources), "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that(".verify_datasources does not throw an error if all datasources are DSConnection objects", {
  datasources <- list(DSConnection$new(), DSConnection$new(), DSConnection$new())
  expect_silent(.verify_datasources(datasources))
})

test_that(".set_datasources returns datasources found by datashield.connections_find() when datasources is NULL", {
  expect_equal(
    with_mocked_bindings(
      .set_datasources(datasources = NULL),
      "datashield.connections_find" = function() conns
    ),
    conns)
})

test_that(".set_datasources returns provided datasources when datasources is not NULL", {
  expect_equal(
    .set_datasources(datasources = conns),
    conns)
})

test_that(".set_datasources throws an error if datasources contain non-DSConnection objects", {
  datasources <- list(DSConnection$new(), DSConnection$new(), "not a DSConnection")
  expect_error(.set_datasources(datasources), "The 'datasources' were expected to be a list of DSConnection-class objects")
})

test_that(".set_datasources does not throw an error if all datasources are DSConnection objects", {
  datasources <- list(DSConnection$new(), DSConnection$new(), DSConnection$new())
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
  expect_error(.check_data_name_length(.data, nfilter.string),
               "The length of string passed to `.data` must be less than 100 characters.")
})

test_that(".check_data_name_length does not throw an error if length of .data is within nfilter.string", {
  .data <- paste(rep("a", 99), collapse = "")
  nfilter.string <- 100
  expect_silent(.check_data_name_length(.data, nfilter.string))
})

test_that(".getEncodeKey returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|"),
    output = c("$LIST$", "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$")
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

