#' @title Keep rows that match a condition
#' @description DataSHIELD implentation of \code{dplyr::filter}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List of expressions that return a logical value, and are defined in terms of the
#' variables in \code{df.name}.
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as an alternative to \code{dplyr::group_by}
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return No return value, called for its side effects. An object (typically a data frame or tibble)
#' with the name specified by \code{newobj} is created on the server.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ds.filter(
#'   df.name = "mtcars",
#'   tidy_expr = list(cyl == 4 & mpg > 20),
#'   newobj = "filtered",
#'   datasources = conns
#' )
#' }
#' @export
ds.filter <- function(df.name = NULL, tidy_expr = NULL, .by = NULL, .preserve = FALSE, newobj = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("filterDS", tidy_expr, list(df.name, .by, .preserve))
  datashield.assign(datasources, newobj, cally)
}
