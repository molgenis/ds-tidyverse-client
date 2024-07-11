#' @title A general vectorised if-else
#' @description DataSHIELD implementation of \code{dplyr::case_when}.
#' @param tidy_select
#' @param default The value used when all of the LHS inputs return either FALSE or NA.
#' @param .ptype An optional prototype declaring the desired output type. If supplied, this overrides the common type of true, false, and missing.
#' @param .size An optional size declaring the desired output size. If supplied, this overrides the size of condition.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return One or more new columns created on the serverside data frame specified in the \code{newobj}.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#'ds.case_when(
#' tidy_select = list(
#'   case_when(
#'     mtcars$mpg < 10 ~ "low",
#'     mtcars$mpg >= 10 & mtcars$mpg < 20 ~ "medium",
#'     mtcars$mpg >= 20 ~ "high")
#'     ),
#'   newobj = "test",
#'   datasources = conns)
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.case_when <- function(tidy_select = NULL, default = NULL, .ptype = NULL, .size = NULL,
                       newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name = NULL, newobj, tidy_select, datasources, check_df = FALSE)
  cally <- .make_serverside_call("case_whenDS", tidy_select, list(default, .ptype, .size))
  datashield.assign(datasources, newobj, cally)
}

