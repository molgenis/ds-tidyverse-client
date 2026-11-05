#' Create a login object for the test servers selected by `getOption("default_driver")`
#'
#' @param assign_method,aggregate_method Passed to `.prepare_dslite()`; unused on Armadillo, which
#'   reads the methods from the installed packages.
#'
#' @return A DataSHIELD login object for DSLite (the default) or Armadillo (`"ArmadilloDriver"`).
#'   Either server holds `.test_tables()`.
.prepare_login <- function(assign_method = NULL, aggregate_method = NULL) {
  if (getOption("default_driver", "DSLiteDriver") == "ArmadilloDriver") {
    return(.prepare_armadillo())
  }
  .prepare_dslite(assign_method, aggregate_method, .test_tables())
}

#' Give the server-side path of a test table
#'
#' @param name A table name from `.test_tables()`.
#'
#' @return The `project/folder/table` path used by both DSLite and Armadillo.
.test_table <- function(name) {
  file.path("tidyverse", "test", name)
}

#' Make the tables the tests read from the servers
#'
#' Uploaded to Armadillo by `data_files/molgenis_armadillo-upload_testing_datasets.R`.
#'
#' @return A named list of data frames.
.test_tables <- function() {
  mtcars <- datasets::mtcars
  mtcars_group <- dplyr::mutate(dplyr::group_by(mtcars, cyl), drop_test = factor("a", levels = c("a", "b")))
  mtcars_group_carb <- dplyr::mutate(dplyr::group_by(mtcars, carb), drop_test = factor("a", levels = c("a", "b")))
  list(
    mtcars = mtcars,
    mtcars_cat_var = dplyr::mutate(mtcars, cat_var = factor(ifelse(mpg > 20, "high", "low"))),
    mtcars_dup_names = cbind(mtcars, dplyr::tibble(cyl = 2)),
    mtcars_group = mtcars_group,
    mtcars_group_carb = mtcars_group_carb,
    mtcars_bad_group = dplyr::group_by(mtcars, qsec),
    test_matrix_values = data.frame(value = 1:20),
    na_df = data.frame(x = c(1, NA, 3, NA, 5), stringsAsFactors = FALSE),
    date_df = data.frame(date_str = c("2024-01-15", "2024-06-30", "2024-12-25"), stringsAsFactors = FALSE),
    date_dmy_df = data.frame(date_str = c("15/01/2024", "30/06/2024", "25/12/2024"), stringsAsFactors = FALSE),
    date_mdy_df = data.frame(date_str = c("01/15/2024", "06/30/2024", "12/25/2024"), stringsAsFactors = FALSE),
    date_dmy_dash_df = data.frame(date_str = c("15-01-2024", "30-06-2024", "25-12-2024"), stringsAsFactors = FALSE),
    date_ymd_slash_df = data.frame(date_str = c("2024/01/15", "2024/06/30", "2024/12/25"), stringsAsFactors = FALSE),
    date_dby_df = data.frame(date_str = c("15 Jan 2024", "30 Jun 2024", "25 Dec 2024"), stringsAsFactors = FALSE),
    date_bdy_df = data.frame(date_str = c("Jan 15, 2024", "Jun 30, 2024", "Dec 25, 2024"), stringsAsFactors = FALSE),
    date_compact_df = data.frame(date_str = c("20240115", "20240630", "20241225"), stringsAsFactors = FALSE),
    date_dot_df = data.frame(date_str = c("15.01.2024", "30.06.2024", "25.12.2024"), stringsAsFactors = FALSE),
    num_range_df = data.frame(x1 = 1:5, x2 = 6:10, x3 = 11:15, y1 = 16:20, stringsAsFactors = FALSE)
  )
}

#' Create a login object for the local Armadillo
#'
#' @return A DataSHIELD login object for three connections to the Armadillo profile
#'   `getOption("armadillo_profile", "default")`.
.prepare_armadillo <- function() {
  loadNamespace("DSMolgenisArmadillo")
  builder <- DSI::newDSLoginBuilder()
  for (server in c("server_1", "server_2", "server_3")) {
    builder$append(
      server = server, url = "http://localhost:8080", user = "admin", password = "admin",
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
