#' @title Create, modify, and delete columns
#' @description DataSHIELD implementation of \code{dplyr::mutate}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param tidy_expr List of tidyselect syntax to be passed to dplyr::mutate.
#' @param .keep Control which columns from \code{df.name} are retained in the output. Options include:
#' \itemize{
#'   \item \code{"all"}: Retains all columns from \code{df.name}. This is the default.
#'   \item \code{"used"}: Retains only the columns used in \code{tidy_expr} to create new columns.
#'   \item \code{"unused"}: Retains only the columns not used in \code{tidy_expr} to create new columns. This is useful if you generate new columns but no longer need the columns used to generate them.
#'   \item \code{"none"}: Doesn't retain any extra columns from \code{df.name}. Only the grouping variables and columns created by \code{tidy_expr} are kept.
#' }
#' Grouping columns and columns created by \code{tidy_expr} are always kept.
#' @param .before <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{tidy_expr} for more details.
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{tidy_expr} for more details.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources datashield connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign
#' @examples
#' \dontrun{
#' ## First log in to a DataSHIELD session with mtcars dataset loaded.
#'
#' ds.mutate(
#'   df.name = "mtcars",
#'   tidy_select = list(mpg_trans = cyl * 1000, new_var = (hp - drat) / qsec),
#'   newobj = "df_with_new_cols"
#' )
#'
#' ## Refer to the package vignette for more examples.
#' }
#' @export
ds.mutate <- function(df.name = NULL, tidy_expr = NULL, newobj = NULL, .keep = "all", .before = NULL,
                      .after = NULL, datasources = NULL) {
  tidy_expr <- .format_args_as_string(rlang::enquo(tidy_expr))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df.name, newobj)
  cally <- .make_serverside_call("mutateDS", tidy_expr, list(df.name, .keep, .before, .after))
  datashield.assign(datasources, newobj, cally)
}
