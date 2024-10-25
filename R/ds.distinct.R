#' @title Keep distinct/unique rows
#' @description DataSHIELD implentation of \code{dplyr::distinct}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr Optionally, list of variables to use when determining uniqueness. If there are multiple rows
#' for a given combination of inputs, only the first row will be preserved. If omitted, will use all
#' variables in the data frame.
#' @param .keep_all If TRUE, keep all variables in .data. If a combination of \code{expr} is not
#' distinct, this keeps the first row of values.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return No return value, called for its side effects. An object (typically a data frame or tibble)
#' with the name specified by \code{newobj} is created on the server.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ds.distinct(
#'   df.name = "mtcars",
#'   expr = list(mpg, cyl),
#'   newobj = "distinct_df"
#' )
#' }
#' @export
ds.distinct <- function(df.name = NULL, tidy_expr = NULL, .keep_all = FALSE, newobj = NULL,
                        datasources = NULL) {
  tidy_expr <- .format_args_as_string(enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("distinctDS", tidy_expr, list(df.name, .keep_all))
  datashield.assign(datasources, newobj, cally)
}
