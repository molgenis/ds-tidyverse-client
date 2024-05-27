#' @title Clientside dplyr rename function
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param .data Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::rename
#' @param newobj Optionally, character specifying name for new server-side data frame. Default is
#' to overwrite original object.
#' @param datasources datashield connections object.
#' @return the object specified by the \code{newobj} argument or
#' as default same name as input object is written to the serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @export
ds.rename <- function(.data = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {
  tidy_select_diffused <- rlang::enquo(tidy_select)
  tidy_select_as_string <- .format_args_as_string(tidy_select_diffused)
  .check_rename_args(.data, newobj)
  datasources <- .set_datasources(datasources)
  newobj <- .set_new_obj(.data, newobj)
  disc_settings <- datashield.aggregate(datasources, call("dsListDisclosureSettings"))
  .check_data_name_length(.data, disc_settings)
  .tidy_disclosure_checks(tidy_select_as_string, disc_settings, datasources)
  args_encoded <- .encode_tidy_eval(tidy_select_as_string, .getEncodeKey())
  cally <- call("renameDS", .data, args_encoded)
  datashield.assign(datasources, newobj, cally)
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
.check_rename_args <- function(.data, newobj) {
  assert_that(is.character(.data))
  assert_that(is.character(newobj))
}
