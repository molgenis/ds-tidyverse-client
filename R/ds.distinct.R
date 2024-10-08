#' @title Keep distinct/unique rows
#' @description DataSHIELD implentation of  \code{dplyr::distinct}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr Optional variables to use when determining uniqueness. If there are multiple rows
#' for a given combination of inputs, only the first row will be preserved. If omitted, will use all
#' variables in the data frame.
#' @param .keep_all If TRUE, keep all variables in .data. If a combination of `expr` is not
#' distinct, this keeps the first row of values.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#'ds.distinct(
#' df.name = "mtcars",
#' expr = list(mpg, cyl),
#' newobj = "distinct_df")
#' }
#' @export
ds.distinct <- function(df.name = NULL, tidy_expr = NULL, .keep_all = FALSE, newobj = NULL,
                        datasources = NULL) {
  tidy_expr <- .format_args_as_string(rlang::enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("distinctDS", tidy_expr, list(df.name, .keep_all))
  datashield.assign(datasources, newobj, cally)
}
