#' Create a login object for the test servers selected by `getOption("default_driver")`
#'
#' @param assign_method,aggregate_method Passed to `.prepare_dslite()`; unused on Armadillo, which
#'   reads the methods from the installed packages.
#' @param tables A named list of tables to be made available at `.test_table(name)`.
#'
#' @return A DataSHIELD login object for DSLite (the default) or Armadillo (`"ArmadilloDriver"`).
.prepare_login <- function(assign_method = NULL, aggregate_method = NULL, tables = list()) {
  if (getOption("default_driver", "DSLiteDriver") == "ArmadilloDriver") {
    return(.prepare_armadillo(tables))
  }
  .prepare_dslite(assign_method, aggregate_method, tables)
}

#' Give the server-side path of a test table
#'
#' @param name The table name, as given in `tables` of `.prepare_login()`.
#'
#' @return The `project/folder/table` path used by both DSLite and Armadillo.
.test_table <- function(name) {
  file.path("tidyverse", "test", name)
}

#' Upload tables to the local Armadillo and create a login object for it
#'
#' @param tables A named list of data frames to upload to project `tidyverse`, folder `test`.
#'   The project is recreated first, so it holds only these tables, as a DSLite server would.
#'
#' @return A DataSHIELD login object for three connections to the Armadillo profile
#'   `getOption("armadillo_profile", "default")`.
.prepare_armadillo <- function(tables) {
  loadNamespace("DSMolgenisArmadillo")
  url <- "http://localhost:8080"
  MolgenisArmadillo::armadillo.login_basic(url, "admin", "admin")
  if ("tidyverse" %in% MolgenisArmadillo::armadillo.list_projects()) {
    MolgenisArmadillo::armadillo.delete_project("tidyverse")
  }
  MolgenisArmadillo::armadillo.create_project("tidyverse")
  for (name in names(tables)) {
    MolgenisArmadillo::armadillo.upload_table("tidyverse", "test", tables[[name]], name)
  }

  builder <- DSI::newDSLoginBuilder()
  for (server in c("server_1", "server_2", "server_3")) {
    builder$append(
      server = server, url = url, user = "admin", password = "admin",
      driver = "ArmadilloDriver", profile = getOption("armadillo_profile", "default")
    )
  }
  builder$build()
}

#' Create a DSLite login object that can be used for testing
#'
#' @param assign_method A string specifying the name of the custom assign method to be added
#'   to the DSLite server. If `NULL`, no additional assign method is added. Default is `NULL`.
#' @param aggregate_method A string specifying the name of the custom aggregate method to be
#'   added to the DSLite server. If `NULL`, no additional aggregate method is added. Default is `NULL`.
#' @param tables A named list of tables to be made available on the DSLite server at `.test_table(name)`.
#'   Default is `list()`.
#'
#' @return A DataSHIELD login object containing the necessary connection information for the DSLite server.
#'
#' @examples
#' \dontrun{
#' # Prepare a DSLite server with default methods and custom assign/aggregate methods
#' login_data <- .prepare_dslite(
#'   assign_method = "customAssign",
#'   aggregate_method = "customAggregate",
#'   tables = list(mtcars = mtcars, mtcars_group = mtcars_group)
#' )
#' }
#' @importFrom DSLite newDSLiteServer
#' @importFrom DSI newDSLoginBuilder
#' @export
.prepare_dslite <- function(assign_method = NULL, aggregate_method = NULL, tables = list()) {
  options(
    datashield.env = environment(),
    datashield.privacyControlLevel = "banana",
    nfilter.tab = 3,
    nfilter.subset = 3,
    nfilter.glm = 0.33,
    nfilter.string = 80,
    nfilter.stringShort = 20,
    nfilter.kNN = 3,
    nfilter.levels.density = 0.33,
    nfilter.levels.max = 40,
    nfilter.noise = 0.25,
    nfilter.privacy.old = 5
  )
  dslite.server <- DSLite::newDSLiteServer(tables = setNames(tables, .test_table(names(tables))))
  dslite.server$config(DSLite::defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
  dslite.server$aggregateMethod("exists", "base::exists")
  dslite.server$aggregateMethod("classDS", "dsBase::classDS")
  dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
  dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")

  if (!is.null(assign_method)) {
    dslite.server$assignMethod(assign_method, paste0("dsTidyverse::", assign_method))
  }

  if (!is.null(aggregate_method)) {
    dslite.server$aggregateMethod(assign_method, paste0("dsTidyverse::", assign_method))
  }

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "server_1", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_2", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_3", url = "dslite.server", driver = "DSLiteDriver")
  login_data <- builder$build()
  return(login_data)
}
