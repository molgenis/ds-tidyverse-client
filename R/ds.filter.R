#' @title Keep rows that match a condition
#' @description DataSHIELD implentation of  \code{dplyr::filter}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param expression Expressions that return a logical value, and are defined in terms of the
#' variables in .data
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' }
#' @export
ds.filter <- function(df.name = NULL, expression = NULL, .preserve = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  cally <- .make_serverside_call("selectDS", tidy_select, list(df.name))
  datashield.assign(datasources, newobj, cally)
}
