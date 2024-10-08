#' @title Keep or drop columns using their names and types.
#' @description DataSHIELD implentation of  \code{dplyr::select}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List of tidyselect syntax to be passed to  \code{dplyr::select}.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign
#' @examples
#'\dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.select(
#'  df.name = "mtcars",
#'  tidy_select = list(mpg, starts_with("t"),
#'  newobj = "df_subset",
#'  dataources = conns)
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.select <- function(df.name = NULL, tidy_expr = NULL, newobj = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(rlang::enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  cally <- .make_serverside_call("selectDS", tidy_expr, list(df.name))
  datashield.assign(datasources, newobj, cally)
}
