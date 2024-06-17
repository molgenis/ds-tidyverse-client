#' @title Clientside dplyr mutate function
#' @description This function is similar to R function \code{mutate}.
#' @details Performs dplyr mutate
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::mutate
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return One or more new columns created on the serverside data frame specified in the \code{newobj}.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @export
ds.mutate <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, .keep = NULL, .before = NULL,
                      .after = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  .check_tidy_args(df.name, newobj, .keep)
  datasources <- .set_datasources(datasources)
  .check_tidy_disclosure(df.name, tidy_select, datasources)
  .call_mutate_ds(tidy_select, df.name, newobj, .keep, .before, .after, datasources)
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
.check_mutate_args <- function(df.name, newobj, .keep) {
  assert_that(!is.null(newobj))
  assert_that(is.character(df.name))
  assert_that(is.character(newobj))
}



#' Call Rename DataShield Function
#'
#' @param tidy_select A tidy selection specification of columns.
#' @param df.name A character string specifying the name of the dataframe.
#' @param newobj A character string specifying the name of the new dataframe after renaming columns.
#' @param datasources A list of Opal connection objects obtained after logging into the Opal servers.
#' @return None.
#' @noRd
.call_mutate_ds <- function(tidy_select, df.name, newobj, .keep, .before, .after, datasources) {
  args_encoded <- .encode_tidy_eval(tidy_select, .get_encode_dictionary())
  cally <- call("mutateDS", df.name, args_encoded, .keep, .before, .after)
  datashield.assign(datasources, newobj, cally)
}

