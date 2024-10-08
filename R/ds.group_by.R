#' @title Group by one or more variables
#' @description DataSHIELD implentation of \code{dplyr::group_by}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr Variables or computations to group by.
#' @param .add 	When FALSE, the default, \code{group_by()} will override existing groups. To add to the
#' existing groups, use .add = TRUE.
#' @param .drop Drop groups formed by factor levels that don't appear in the data? The default is
#' TRUE except when .data has been previously grouped with .drop = FALSE.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign
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
  tidy_expr <- .format_args_as_string(rlang::enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("groupByDS", tidy_expr, list(df.name, .add, .drop))
  datashield.assign(datasources, newobj, cally)
}

#' @title Remove grouping
#' @description DataSHIELD implentation of \code{dplyr::ungroup}.
#' @param x a tibble.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
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
