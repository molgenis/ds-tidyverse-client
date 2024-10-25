#' @title Bind multiple data frames by column
#' @description DataSHIELD implementation of \code{dplyr::bind_cols}.
#' @param to_combine 	Data frames to combine. Each argument can either be a data frame, a list that
#' could be a data frame, or a list of data frames. Columns are matched by name, and any missing
#' columns will be filled with NA.
#' @param .name_repair One of "unique", "universal", or "check_unique". See
#' \code{vctrs::vec_as_names()} for the meaning of these options.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return No return value, called for its side effects. A data frame with the name specified by
#' \code{newobj} and the same type as the first element of \code{to_combine} is created on the
#' server.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.bind_cols(
#'   to_combine = list(mtcars, mtcars),
#'   .name_repair = "universal",
#'   newobj = "test",
#'   datasources = conns
#' )
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.bind_cols <- function(to_combine = NULL, .name_repair = c("unique", "universal", "check_unique", "minimal"), newobj = NULL, datasources = NULL) {
  to_combine <- .format_args_as_string(enquo(to_combine))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(NULL, newobj, check_df = FALSE)
  cally <- .make_serverside_call("bindColsDS", to_combine, list(.name_repair))
  datashield.assign(datasources, newobj, cally)
}
