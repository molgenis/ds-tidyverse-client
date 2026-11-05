#' @title Count the observations in each group
#' @description DataSHIELD implementation of \code{dplyr::count}.
#' @details Missing values are counted as their own level. The server returns an error if any
#' count is between 1 and \code{nfilter.tab} - 1.
#' @param .data Character specifying a server-side data frame or tibble.
#' @param ... Names of the columns to count by.
#' @param .pooled Logical; if \code{TRUE}, add counts summed across cohorts, with
#' \code{cohort = "pooled"}.
#' @param datasources DataSHIELD connections object.
#' @return A tibble with a \code{cohort} column identifying the server, one column per variable in
#' \code{...} and a column \code{n} of counts.
#' @importFrom DSI datashield.aggregate
#' @importFrom rlang enquos
#' @examples
#' \dontrun{
#' ds.count("df", sex, smoking, datasources = conns)
#' }
#' @export
ds.count <- function(.data = NULL, ..., .pooled = FALSE, datasources = NULL) {
  tidy_expr <- .format_dots_as_string(enquos(...))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(.data, NULL, check_obj = FALSE)
  cally <- .make_serverside_call("countDS", tidy_expr, list(.data))
  out <- .bind_cohorts(datashield.aggregate(datasources, cally))
  if (.pooled) {
    out <- dplyr::bind_rows(out, .pool_counts(out))
  }
  return(out)
}

#' Sum counts across cohorts
#'
#' @param counts The output of \code{ds.count}, with a column \code{n}.
#' @return A tibble of counts summed over cohorts, with \code{cohort = "pooled"}.
#' @importFrom rlang .data
#' @noRd
.pool_counts <- function(counts) {
  counts |>
    dplyr::summarise(n = sum(.data$n), .by = -c("cohort", "n")) |>
    dplyr::mutate(cohort = "pooled", .before = 1)
}
