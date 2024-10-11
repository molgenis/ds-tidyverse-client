#' @title Vectorised if-else
#' @description DataSHIELD implementation of \code{dplyr::if_else}.
#' @param condition A list specifying a logical vector in tidyverse syntax, ie data and column names unquoted.
#' @param true Vector to use for TRUE value of condition.
#' @param false Vector to use for FALSE value of condition.
#' @param missing If not NULL, will be used as the value for NA values of condition. Follows the same size and type rules as true and false.
#' @param ptype An optional prototype declaring the desired output type. If supplied, this overrides the common type of true, false, and missing.
#' @param size An optional size declaring the desired output size. If supplied, this overrides the size of condition.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return One or more new columns created on the serverside data frame specified in the \code{newobj}.
#' @importFrom DSI datashield.assign
#' @importFrom rlang enquo
#' @examples
#' \dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
# ` ds.if_else(
# `  condition = list(mtcars$mpg > 20),
# `. true = "high",
# `. false = "low",
# `. newobj = "new_var")
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.if_else <- function(condition = NULL, true = NULL, false = NULL, missing = NULL,
                       ptype = NULL, size = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(enquo(condition))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(NULL, newobj, check_df = FALSE)
  cally <- .make_serverside_call("ifElseDS", tidy_select, list(true, false, missing, ptype, size))
  datashield.assign(datasources, newobj, cally)
}
