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
ds.select <- function(.data = NULL, newobj = NULL, datasources = NULL, select_args = NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  expr <- rlang::enquo(select_args) ## Get the unquoted arguments
  expr_squashed <- quo_text(expr) ## Convert them to a string
  expr_as_string <- str_replace_all(expr_squashed, fixed("("), "$LB$")
  expr_as_string <- str_replace_all(expr_as_string, fixed(")"), "$RB$")
  expr_as_string <- str_replace_all(expr_as_string, fixed('\"'), "$QUOTE$")
  expr_as_string <- str_replace_all(expr_as_string, fixed('\"'), "$QUOTE$")
  expr_as_string <- str_replace_all(expr_as_string, fixed(','), "$COMMA$")
  expr_as_string <- str_replace_all(expr_as_string, fixed(' '), "$SPACE$")

  cally <- call("selectDS", .data, expr_as_string)
  DSI::datashield.assign(datasources, newobj, cally)
}
