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
  ## Take arguments provided in a list and convert to a character vector
  tidy_select_diffused <- rlang::enquo(tidy_select)
  tidy_select_as_string <- .format_args_as_string(tidy_select_diffused)

  ## Check arguments valid
  .check_select_args(.data, newobj)

  ## Set defaults if not set
  datasources <- .set_datasources(datasources)
  newobj <- .set_new_obj(.data, newobj)

  ## Check disclosure issues
  disc_settings <- datashield.aggregate(datasources, call("dsListDisclosureSettings"))
  # disc_settings <- list_disclosure_settings()
  .check_data_name_length(.data, disc_settings, datasources)
  .tidy_disclosure_checks(tidy_select_as_string, disc_settings, datasources)

  ## Encode arguments to pass R parser
  args_encoded <- .encode_tidy_eval(tidy_select_as_string, .getEncodeKey())

  ## Send arguments to serverside package
  cally <- call("selectDS", .data, args_encoded)
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
.check_select_args <- function(.data, newobj) {
  assert_that(is.character(.data))
  assert_that(is.character(newobj))
}
