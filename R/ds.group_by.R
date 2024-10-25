#' @title Group by one or more variables
#' @description DataSHIELD implentation of \code{dplyr::group_by}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List of variables or computations to group by.
#' @param .add 	When FALSE, the default, \code{group_by()} will override existing groups. To add to the
#' existing groups, use .add = TRUE.
#' @param .drop Drop groups formed by factor levels that don't appear in the data? The default is
#' TRUE except when .data has been previously grouped with .drop = FALSE.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return No return value, called for its side effects. A grouped data frame with class grouped_df
#' \code{newobj} is created on the server, unless the combination of \code{tidy_expr} and \code{.add}
#' yields a empty set of grouping columns, in which case a tibble will be created on the server.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ds.group_by(
#'   df.name = "mtcars",
#'   expr = list(mpg, cyl),
#'   newobj = "grouped_df"
#' )
#' }
#' @export
ds.group_by <- function(df.name = NULL, tidy_expr, .add = FALSE, .drop = TRUE, newobj = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("groupByDS", tidy_expr, list(df.name, .add, .drop))
  datashield.assign(datasources, newobj, cally)
}

#' @title Remove grouping from a tibble or data frame
#' @description DataSHIELD implentation of \code{dplyr::ungroup}.
#' @param x a tibble or data frame.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return No return value, called for its side effects. An ungrouped data frame or tibble is
#' created on the server.
#' @examples
#' \dontrun{
#' ds.ungroup("grouped_df")
#' }
#' @export
ds.ungroup <- function(x = NULL, newobj = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  .check_tidy_args(x, newobj)
  cally <- .make_serverside_call("ungroupDS", tidy_select = NULL, list(x))
  datashield.assign(datasources, newobj, cally)
}
