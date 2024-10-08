#' @title Coerce a data frame or matrix to a tibble.
#' @description DataSHIELD implementation of \code{tibble::as_tibble}. Currently only implemented
#' for data frames and tibbles.
#' @param x A data frame or matrix.
#' @param .rows The number of rows, useful to create a 0-column tibble or just as an additional
#' check.
#' @param .name_repair Treatment of problematic column names:
#'   \itemize{
#'     \item "minimal": No name repair or checks, beyond basic existence.
#'     \item "unique": Make sure names are unique and not empty.
#'     \item "check_unique": (default value), no name repair, but check they are unique.
#'     \item "universal": Make the names unique and syntactic.
#'   }
#' @param rownames How to treat existing row names of a data frame or matrix:
#'   \itemize{
#'     \item `NULL`: remove row names. This is the default.
#'     \item `NA`: keep row names.
#'     \item A string: the name of a new column. Existing rownames are transferred
#'     into this column and the \code{row.names} attribute is deleted.
#'     No name repair is applied to the new column name, even if `x` already contains
#'     a column of that name.
#'   }
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return A serverside tibble with the name specified in the \code{newobj}.
#' @importFrom DSI datashield.assign
#' @examples
#' \dontrun{
#' ds.as_tibble(
#'   x = "mtcars",
#'   newobj = "mtcars_tib",
#'   datasources = conns
#' )
#' }
#' @export
ds.as_tibble <- function(x = NULL, .rows = NULL, .name_repair = "check_unique", rownames = NULL,
                         newobj = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  .check_tidy_args(x, newobj)
  cally <- .make_serverside_call("asTibbleDS", NULL, list(x, .rows, .name_repair, rownames))
  datashield.assign(datasources, newobj, cally)
}
