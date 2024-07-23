#' @title Bind multiple data frames by row.
#' @description DataSHIELD implementation of \code{dplyr::bind_rows}.
#' @param to_combine 	Data frames to combine. Each argument can either be a data frame, a list that
#' could be a data frame, or a list of data frames. Columns are matched by name, and any missing
#' columns will be filled with NA.
#' @param .id he name of an optional identifier column. Provide a string to create an output column
#' that identifies each input. The column will use names if available, otherwise it will use
#' positions.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return A serverside data frame with name specified in \code{newobj} and the same type as the
#' first element of `to_combine`.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#'ds.bind_rows(
#' to_combine = list(mtcars, mtcars),
#' newobj = "test",
#' datasources = conns)
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.bind_rows <- function(to_combine = NULL, .id = NULL, newobj = NULL, datasources = NULL) {
  to_combine <- .format_args_as_string(rlang::enquo(to_combine))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name = NULL, newobj, to_combine, datasources, check_df = FALSE)
  cally <- .make_serverside_call("bindRowsDS", to_combine, list(.id))
  datashield.assign(datasources, newobj, cally)
}

