#' @title Clientside dplyr rename function
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::rename
#' @param newobj Optionally, character specifying name for new server-side data frame. Default is
#' to overwrite original object.
#' @param datasources datashield connections object.
#' @return the object specified by the \code{newobj} argument or
#' as default same name as input object is written to the serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.rename(
#'  df.name = "mtcars",
#'  tidy_select = list(new_var_1 = mpg, new_var_2 = cyl),
#'  newobj = "df_renamed",
#'  dataources = conns)
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.rename <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  cally <- .make_serverside_call("renameDS", tidy_select, list(df.name))
  datashield.assign(datasources, newobj, cally)
}
