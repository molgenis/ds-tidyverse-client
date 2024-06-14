#' @title Clientside dplyr select function
#' @description This function is similar to R function \code{select}.
#' @details Performs dplyr select
#' @param .data Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::select.
#' @param newobj Optionally, character specifying name for new server-side data frame. Default is
#' to overwrite original object.
#' @param datasources datashield connections object.
#' @return the object specified by the \code{newobj} argument or
#' as default same name as input object is written to the serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @export
ds.select <- function(.data = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  .check_select_args(.data, newobj)
  datasources <- .set_datasources(datasources)
  newobj <- .set_new_obj(.data, newobj)
  .check_select_disclosure(.data, tidy_select, datasources)
  .call_select_ds(tidy_select, .data, newobj, datasources)
}

#' Check Select Arguments
#'
#' @param .data Character specifying a serverside data frame or tibble.
#' @param newobj Optionally, character specifying name for new server-side data frame.
#' @return This function does not return a value but is used for argument validation.
#'
#' @importFrom assertthat assert_that
#'
#' @noRd
.check_select_args <- function(.data, newobj) {
  assert_that(is.character(.data))
  assert_that(is.character(newobj))
}

.check_select_disclosure <- function(.data, tidy_select, datasources) {
  disc_settings <- datashield.aggregate(datasources, call("dsListDisclosureSettingsTidyVerse"))
  .check_data_name_length(.data, disc_settings)
  .check_tidy_disclosure(tidy_select, disc_settings, datasources)
}

.call_select_ds <- function(tidy_select, .data, newobj, datasources) {
  args_encoded <- .encode_tidy_eval(tidy_select, .get_encode_dictionary())
  cally <- call("selectDS", .data, args_encoded)
  datashield.assign(datasources, newobj, cally)
}
