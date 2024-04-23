#'
#' @title Clientside dplyr select function
#' @description This function is similar to R function \code{select}.
#' @details Performs dplyr select
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param newobj Optional, character specifying name of new object
#' @param datasources datashield connections object
#' @param select_args Arguments normally passed to `...` in dplyr::select
#' @return the object specified by the \code{newobj} argument or
#' of \code{ds.select} (or as default same name as input object) which is written to the serverside.
#' @importFrom tidyselect eval_select
#' @importFrom rlang set_names quo_squash
#' @importFrom DSI datashield.connections_find
#' @export
ds.select <- function(.data = NULL, tidy_select = NULL, newobj = NULL, datasources = NULL) {

  ## Take arguments provided in a list and convert to a character vector
  tidy_select_diffused <- rlang::enquo(tidy_select)
  tidy_select_as_string <- .format_args_as_string(tidy_select_diffused)

  ## Check arguments valid
  .check_args_type(.data, newobj)

  ## Set defaults if not set
  datasources <- .set_datasources(datasources)
  newobj <- .set_new_obj(newobj)

  ## Check disclosure settings
  disc_settings <- dsBase::listDisclosureSettingsDS()
  .ds_disclosure_checks(.data, nfilter.string)
  .tidy_disclosure_checks(args_as_string, disc_settings$nfilter.string)

  ## Encode arguments to pass R parser
  args_encoded <- .encode_tidy_eval(args_as_string)

  ## Send arguments to serverside package
  cally <- call("selectDS", .data, args_encoded)
  warnings <- DSI::datashield.assign(datasources, newobj, cally)

  ## Process any messages returned
  messages <- .check_object_creation(datasources, newobj)
  if(messages$success){
    return(messages$messages %>% walk(cli_alert_success))
  } else {
    return(messages$messages %>% walk(cli_alert_failure))
  }
}

.check_args_type <- function(.data, tidy_select, newobj){

  checkmate::check_character(.data)
  checkmate::check_character(newobj)

}
