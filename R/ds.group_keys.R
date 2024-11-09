#' @title Describe the groups within a grouped tibble or data frame
#' @description DataSHIELD implentation of \code{dplyr::group_keys}.
#' @param df.name Character specifying a serverside tibble.
#' @param datasources DataSHIELD connections object.
#' @return A data frame describing the groups.
#' @importFrom DSI datashield.aggregate
#' @examples
#' \dontrun{
#' my_groups <- ds.group_keys("grouped_df")
#' }
#' @export
ds.group_keys <- function(df.name = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, NULL, check_obj = F)
  cally <- .make_serverside_call("groupKeysDS", tidy_select = NULL, list(df.name))
  datashield.aggregate(datasources, cally)
}
