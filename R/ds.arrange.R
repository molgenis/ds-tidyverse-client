#' @title Order the rows of a data frame by the values of selected columns
#' @description DataSHIELD implentation of  \code{dplyr::arrange}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param expr Variables, or functions of variables. Use desc() to sort a variable in descending
#' order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames
#' only.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#'ds.arrange(
#'  "mtcars",
#'  expr = list(drat),
#'  newobj = "sorted_df"
#'  )
#' }
#' @export
ds.arrange <- function(df.name = NULL, expr = NULL, .by_group = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(expr))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  cally <- .make_serverside_call("arrangeDS", tidy_select, list(df.name, .by_group))
  datashield.assign(datasources, newobj, cally)
}
