#' @title Coerce a data frame or matrix to a tibble. Currently not implemented for lists or other
#' object types.
#' @description DataSHIELD implementation of \code{tibble::as_tibble}.
#' @param x A data frame or matrix.
#' @param .rows The number of rows, useful to create a 0-column tibble or just as an additional
#' check.
#' @param .name_repair Treatment of problematic column names:
#'   * "minimal": No name repair or checks, beyond basic existence,\
#'   * "unique": Make sure names are unique and not empty,
#'   * "check_unique": (default value), no name repair, but check they are unique,
#'   * "universal": Make the names unique and syntactic
#' @param rownames How to treat existing row names of a data frame or matrix:
#'   * `NULL`: remove row names. This is the default.
#'   * `NA`: keep row names.
#'   * A string: the name of a new column. Existing rownames are transferred
#'     into this column and the `row.names` attribute is deleted.
#'     No name repair is applied to the new column name, even if `x` already contains
#'     a column of that name.
#' @param column_name Optionally, specify column names of the new object.
#' @return A serverside tibble with the name specified in the \code{newobj}.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' }
#' @export
ds.as_tibble <- function(x = NULL, .rows = NULL, .name_repair = "check_unique", rownames = NULL,
                         column_name = NULL, newobj = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name = x, newobj = newobj, tidy_select = NULL, datasources)
  cally <- .make_serverside_call("asTibbleDS", NULL, list(x, .rows, .name_repair, rownames, column_name))
  datashield.assign(datasources, newobj, cally)
}
