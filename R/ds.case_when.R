#' @title A general vectorised if-else
#' @description DataSHIELD implementation of \code{dplyr::case_when}.
#' @param dynamic_dots A sequence of two-sided formulas. The left hand side (LHS) determines which
#' values match this case. The right hand side (RHS) provides the replacement value.
#' The LHS inputs must evaluate to logical vectors.
#' The RHS inputs will be coerced to their common type.
#' All inputs will be recycled to their common size. That said, we encourage all LHS inputs to be
#' the same size. Recycling is mainly useful for RHS inputs, where you might supply a size 1 input
#' that will be recycled to the size of the LHS inputs.
#' NULL inputs are ignored.
#' @param .default The value used when all of the LHS inputs return either FALSE or NA.
#' @param .ptype An optional prototype declaring the desired output type. If supplied, this overrides the common type of true, false, and missing.
#' @param .size An optional size declaring the desired output size. If supplied, this overrides the size of condition.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return One or more new columns created on the serverside data frame specified in the \code{newobj}.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#' \dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.case_when(
#'   tidy_select = list(
#'     mtcars$mpg < 10 ~ "low",
#'     mtcars$mpg >= 10 & mtcars$mpg < 20 ~ "medium",
#'     mtcars$mpg >= 20 ~ "high"
#'   ),
#'   newobj = "test",
#'   datasources = conns
#' )
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.case_when <- function(dynamic_dots = NULL, .default = NULL, .ptype = NULL, .size = NULL,
                         newobj = NULL, datasources = NULL) {
  dynamic_dots <- .format_args_as_string(rlang::enquo(dynamic_dots))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name = NULL, newobj, dynamic_dots, datasources, check_df = FALSE)
  cally <- .make_serverside_call("caseWhenDS", dynamic_dots, list(.default, .ptype, .size))
  datashield.assign(datasources, newobj, cally)
}
