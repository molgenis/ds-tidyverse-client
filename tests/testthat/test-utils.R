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
