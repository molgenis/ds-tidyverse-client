#' @title Clientside dplyr rename function
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::rename
#' @param newobj Optionally, character specifying name for new server-side data frame. Default is
#' to overwrite original object.
#' @param datasources datashield connections object.
#' @return the object specified by the \code{newobj} argument or
#' as default same name as input object is written to the serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @export
ds.rename <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  .check_rename_args(df.name, newobj)
  datasources <- .set_datasources(datasources)
  .check_tidy_disclosure(df.name, tidy_select, datasources)
  .call_rename_ds(tidy_select, df.name, newobj, datasources)
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
  assert_that(!is.null(newobj))
  assert_that(is.character(.data))
  assert_that(is.character(newobj))
}

#' Check Rename Disclosure Settings
#'
#' @param df.name A character string specifying the name of the dataframe.
#' @param tidy_select A tidy selection specification of columns.
#' @param datasources A list of Opal connection objects obtained after logging into the Opal servers.
#' @return None. This function is used for its side effects of checking disclosure settings.
#' @noRd
.check_rename_disclosure <- function(df.name, tidy_select, datasources) {
  disc_settings <- datashield.aggregate(datasources, call("dsListDisclosureSettingsTidyVerse"))
  .check_data_name_length(df.name, disc_settings)
  .check_tidy_disclosure(tidy_select, disc_settings, datasources)
}

#' Call Rename DataShield Function
#'
#' @param tidy_select A tidy selection specification of columns.
#' @param df.name A character string specifying the name of the dataframe.
#' @param newobj A character string specifying the name of the new dataframe after renaming columns.
#' @param datasources A list of Opal connection objects obtained after logging into the Opal servers.
#' @return None.
#' @noRd
.call_rename_ds <- function(tidy_select, df.name, newobj, datasources) {
  args_encoded <- .encode_tidy_eval(tidy_select, .get_encode_dictionary())
  cally <- call("renameDS", df.name, args_encoded)
  datashield.assign(datasources, newobj, cally)
}

