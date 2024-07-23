#' @title Clientside dplyr select function
#' @description This function is similar to R function \code{dplyr::select}.
#' @details Performs dplyr select
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::select.
#' @param newobj Optionally, character specifying name for new server-side data frame. Default is
#' to overwrite original object.
#' @param datasources datashield connections object.
#' @return The object specified by the \code{newobj} argument or
#' as default same name as input object is written to the serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
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
ds.select <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  cally <- .make_serverside_call("selectDS", tidy_select, list(df.name))
  datashield.assign(datasources, newobj, cally)
}
