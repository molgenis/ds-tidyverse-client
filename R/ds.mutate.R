#' @title Clientside dplyr mutate function
#' @description DataSHIELD implementation of \code{dplyr::mutate}.
#' @details Performs dplyr mutate
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of Tidyselect syntax to be passed to dplyr::mutate
#' @param .keep Control which columns from .data are retained in the output. Grouping columns and
#' columns created by ... are always kept. "all" retains all columns from .data. This is the default.
#' "used" retains only the columns used in `tidy_select` to create new columns. "unused" retains
#' only the columns not used in `tidy_select` to create new columns. This is useful if you generate
#' new columns, but no longer need the columns used to generate them. "none" doesn't retain any
#' extra columns from `df.name`. Only the grouping variables and columns created by `tidy_select`
#' are kept.
#' @param .before <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See `relocate` for more details.
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See `relocate` for more details.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return One or more new columns created on the serverside data frame specified in the \code{newobj}.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @export
ds.mutate <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, .keep = NULL, .before = NULL,
                      .after = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  .check_tidy_disclosure(df.name, tidy_select, datasources)
  .call_tidy_ds("mutateDS", df.name, tidy_select, list(.keep, .before, .after), newobj, datasources)
}
