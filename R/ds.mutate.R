#' @title Create, modify, and delete columns
#' @description DataSHIELD implementation of \code{dplyr::mutate}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_select List of tidyselect syntax to be passed to dplyr::mutate.
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
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.mutate(
#'  df.name = "mtcars",
#'  tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
#'  newobj = "df_with_new_cols")
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.mutate <- function(df.name = NULL, tidy_select = NULL, newobj = NULL, .keep = "all", .before = NULL,
                      .after = NULL, datasources = NULL) {
  tidy_select <- .format_args_as_string(rlang::enquo(tidy_select))
  datasources <- .set_datasources(datasources)
  .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  cally <- .make_serverside_call("mutateDS", tidy_select, list(df.name, .keep, .before, .after))
  datashield.assign(datasources, newobj, cally)
}
