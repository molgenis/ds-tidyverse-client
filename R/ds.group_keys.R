#' @title Describing the groups within a grouped tibble
#' @description DataSHIELD implentation of \code{dplyr::group_keys}.
#' @param df.name Character specifying a serverside tibble.
#' @param datasources DataSHIELD connections object.
#' @return A tibble describing the groups
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#'  ds.filter(
#' }
#' @export
ds.group_keys <- function(df.name = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, datasources)
  cally <- .make_serverside_call("groupKeysDS", list(df.name))
  datashield.aggregate(datasources, newobj, cally)
}
