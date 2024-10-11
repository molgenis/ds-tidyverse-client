#' @title Order the rows of a data frame by the values of selected columns
#' @description DataSHIELD implentation of \code{dplyr::arrange}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr A list containing variables, or functions of variables. Use \code{desc()} to sort a
#' variable in descending order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames
#' only.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ds.arrange(
#'   df.name = "mtcars",
#'   tidy_expr = list(drat),
#'   newobj = "sorted_df",
#'   datasources = conns
#' )
#' }
#' @export
ds.arrange <- function(df.name = NULL, tidy_expr = NULL, .by_group = NULL, newobj = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("arrangeDS", tidy_expr, list(df.name, .by_group))
  datashield.assign(datasources, newobj, cally)
}
