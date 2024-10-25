#' @title Keep or drop columns using their names and types
#' @description DataSHIELD implentation of \code{dplyr::select}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List of one or more unquoted expressions separated by commas. Variable names can
#' be used as if they were positions in the data frame, so expressions like x:y can be used to
#' select a range of variables.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return No return value, called for its side effects. An object (typically a data frame or tibble)
#' with the name specified by \code{newobj} is created on the server.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ds.select(
#'   df.name = "mtcars",
#'   tidy_expr = list(mpg, starts_with("t")),
#'   newobj = "df_subset",
#'   dataources = conns
#' )
#' }
#' @export
ds.select <- function(df.name = NULL, tidy_expr = NULL, newobj = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  cally <- .make_serverside_call("selectDS", tidy_expr, list(df.name))
  datashield.assign(datasources, newobj, cally)
}
