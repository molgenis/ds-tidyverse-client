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

  datasources <- .set_datasources(datasources)
  .verify_datasources(datasources)

  args_as_quo <- rlang::enquo(tidy_select)
  args_as_string <- .format_args_as_string(args_as_quo)
  string_encoded <- .encode_tidy_eval(args_as_string, .getEncodeKey())

  cally <- call("selectDS", .data, string_encoded)
  warnings <- DSI::datashield.assign(datasources, newobj, cally)
  ## Write functionality to handle warnings

  messages <- .check_object_creation(datasources, newobj)
  if(messages$success){
    return(messages$messages %>% walk(cli_alert_success))
  } else {
    return(messages$messages %>% walk(cli_alert_failure))
  }
}
