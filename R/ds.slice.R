#' @title Subset rows using their positions
#' @description DataSHIELD implentation of  \code{dplyr::slice}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List, provide either positive values to keep, or negative values to drop. The values
#' provided must be either all positive or all negative. Indices beyond the number of rows in the
#' input are silently ignored.
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{dplyr::group_by}
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign
#' @examples
#'\dontrun{
#'ds.slice(
#' df.name = "mtcars",
#' expr = list(1:10),
#' .by = "cyl",
#' newobj = "sliced_df")
#' }
#' @export
ds.slice <- function(df.name = NULL, tidy_expr = NULL, .by = NULL, .preserve = FALSE, newobj = NULL,
                     datasources = NULL) {
  tidy_expr <- .format_args_as_string(rlang::enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("sliceDS", tidy_expr, list(df.name, .by, .preserve))
  datashield.assign(datasources, newobj, cally)
}
