#' @title Summarise each group down to one row
#' @description DataSHIELD implementation of \code{dplyr::summarise}.
#' @details The functions permitted on the server are \code{n}, \code{mean}, \code{sd}, \code{var},
#' \code{median}, \code{quantile}, \code{sum}, \code{is.na}, \code{!}, \code{across}, \code{c},
#' \code{list} and \code{~}. \code{quantile()} takes a single probability from 0.05, 0.1, 0.25,
#' 0.5, 0.75, 0.9 and 0.95.
#'
#' With \code{.shape = "long"}, each column \code{<variable>_<statistic>}, as named by
#' \code{across()}, is split into \code{variable} and \code{stat_name} at the last underscore. A
#' column name without an underscore, such as \code{n}, gives \code{variable = NA}. Each
#' statistic name must be one of those recognised for pooling, listed below.
#'
#' With \code{.pooled = TRUE}, rows with \code{cohort = "pooled"} are added. Statistics are
#' recognised by name: \code{n} is summed; \code{mean} is weighted by \code{n}; \code{sd} and
#' \code{var} are pooled exactly from \code{n}, \code{mean} and \code{sd} or \code{var};
#' \code{median} and \code{p05}, \code{p10}, \code{p25}, \code{p50}, \code{p75}, \code{p90} and
#' \code{p95} are weighted by \code{n}, as in \code{dsBaseClient::ds.quantileMean}, which is an
#' approximation. Each variable therefore needs an \code{n} statistic counting its non-missing
#' values, e.g. \code{n = ~sum(!is.na(.x))}. Groups are taken from \code{.by}, so a grouped data
#' frame should be summarised with \code{.by} instead when \code{.shape = "long"} or
#' \code{.pooled = TRUE}.
#' @param .data Character specifying a server-side data frame or tibble.
#' @param ... Name-value pairs of summary functions, as in \code{dplyr::summarise}.
#' @param .by Optionally, a character vector of columns to group by for just this operation.
#' @param .shape Either \code{"wide"}, dplyr's own shape, or \code{"long"}, with one row per cohort,
#' group, variable and statistic.
#' @param .pooled Logical; if \code{TRUE}, add statistics pooled across cohorts.
#' @param datasources DataSHIELD connections object.
#' @return A tibble with a \code{cohort} column identifying the server.
#' @importFrom DSI datashield.aggregate
#' @importFrom rlang enquos
#' @examples
#' \dontrun{
#' ds.summarise(
#'   "df",
#'   across(c(bmi, age), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd)),
#'   .by = "sex",
#'   .shape = "long",
#'   .pooled = TRUE,
#'   datasources = conns
#' )
#' }
#' @export
ds.summarise <- function(.data = NULL, ..., .by = NULL, .shape = c("wide", "long"), .pooled = FALSE,
                         datasources = NULL) {
  .shape <- match.arg(.shape)
  tidy_expr <- .format_dots_as_string(enquos(...))
  datasources <- .set_datasources(datasources)
  .check_tidy_args(.data, NULL, check_obj = FALSE)
  cally <- .make_serverside_call("summariseDS", tidy_expr, list(.data, .by))
  out <- .bind_cohorts(datashield.aggregate(datasources, cally))
  if (.shape == "wide" && !.pooled) {
    return(out)
  }
  long <- .pivot_summaries_longer(out, .by)
  if (.pooled) {
    long <- dplyr::bind_rows(long, .pool_summaries(long, .by))
  }
  if (.shape == "wide") {
    return(.pivot_summaries_wider(long))
  }
  return(long)
}

#' Convert captured dots to a string of comma-separated arguments
#'
#' @param dots A list of quosures, from \code{rlang::enquos(...)}.
#' @return A string such as \code{"m = mean(x), sd(x)"}.
#' @importFrom rlang quo_text
#' @noRd
.format_dots_as_string <- function(dots) {
  exprs <- vapply(dots, quo_text, character(1))
  arg_names <- names(dots)
  paste(ifelse(arg_names == "", exprs, paste(arg_names, "=", exprs)), collapse = ", ")
}

#' Bind the results from each server into one tibble
#'
#' @param results A named list of data frames, one per server.
#' @return A tibble with a \code{cohort} column holding the server names.
#' @noRd
.bind_cohorts <- function(results) {
  dplyr::as_tibble(purrr::list_rbind(results, names_to = "cohort"))
}

#' Pivot summaries to one row per cohort, group, variable and statistic
#'
#' @param out The wide output of \code{ds.summarise}.
#' @param .by A character vector of grouping columns.
#' @return A tibble with columns \code{cohort}, the grouping columns, \code{variable},
#' \code{stat_name} and \code{stat}.
#' @noRd
.pivot_summaries_longer <- function(out, .by) {
  .check_stat_names(setdiff(names(out), c("cohort", .by)))
  out |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("cohort", .by)),
      names_to = c("variable", "stat_name"),
      names_pattern = "^(?:(.*)_)?([^_]+)$",
      values_to = "stat"
    ) |>
    dplyr::mutate(variable = dplyr::na_if(.data$variable, ""))
}

#' Get the statistic names recognised by the long shape and by pooling
#'
#' @return A character vector of statistic names.
#' @noRd
.recognised_stats <- function() {
  c("n", "mean", "sd", "var", "median", "p05", "p10", "p25", "p50", "p75", "p90", "p95")
}

#' Check that each column name ends in a recognised statistic name
#'
#' @param columns The names of the summary columns.
#' @return None. Throws an error naming the columns that do not.
#' @importFrom cli cli_abort
#' @noRd
.check_stat_names <- function(columns) {
  recognised <- .recognised_stats()
  unknown <- columns[!sub("^.*_", "", columns) %in% recognised]
  if (length(unknown) > 0) {
    cli_abort(c(
      "With {.code .shape = \"long\"} or {.code .pooled = TRUE}, column names must end in a recognised statistic.",
      "i" = "Recognised statistics are {.val {recognised}}.",
      "x" = "{.val {unknown}} {?does/do} not."
    ))
  }
}

#' Pivot long summaries back to one column per variable and statistic
#'
#' @param long The long output of \code{ds.summarise}.
#' @return A tibble with one row per cohort and group.
#' @noRd
.pivot_summaries_wider <- function(long) {
  long |>
    dplyr::mutate(
      name = dplyr::if_else(is.na(.data$variable), .data$stat_name, paste(.data$variable, .data$stat_name, sep = "_"))
    ) |>
    dplyr::select(-"variable", -"stat_name") |>
    tidyr::pivot_wider(names_from = "name", values_from = "stat")
}

#' Pool long summaries across cohorts
#'
#' @param long The long output of \code{ds.summarise}.
#' @param .by A character vector of grouping columns.
#' @return A tibble of pooled statistics with \code{cohort = "pooled"}, in the shape of \code{long}.
#' @importFrom rlang .data
#' @noRd
.pool_summaries <- function(long, .by) {
  .check_poolable(long)
  keys <- c("cohort", .by, "variable")
  weights <- dplyr::filter(long, .data$stat_name == "n") |> dplyr::select(dplyr::all_of(keys), n = "stat")
  means <- dplyr::filter(long, .data$stat_name == "mean") |> dplyr::select(dplyr::all_of(keys), mean = "stat")
  long |>
    dplyr::left_join(weights, by = keys) |>
    dplyr::left_join(means, by = keys) |>
    dplyr::summarise(
      stat = .pool_stat(dplyr::first(.data$stat_name), .data$stat, .data$n, .data$mean),
      .by = dplyr::all_of(c(.by, "variable", "stat_name"))
    ) |>
    dplyr::mutate(cohort = "pooled", .before = 1)
}

#' Pool one statistic across cohorts
#'
#' @param stat_name The name of the statistic.
#' @param stat A numeric vector of the statistic, one value per cohort.
#' @param n A numeric vector of non-missing counts, one per cohort.
#' @param mean A numeric vector of means, one per cohort, or NA if there are none.
#' @return The pooled statistic.
#' @importFrom stats weighted.mean
#' @noRd
.pool_stat <- function(stat_name, stat, n, mean) {
  if (stat_name == "n") {
    return(sum(stat))
  }
  has_data <- n > 0
  stat <- stat[has_data]
  n <- n[has_data]
  mean <- mean[has_data]
  if (stat_name == "var") {
    return(.pool_variance(stat, n, mean))
  }
  if (stat_name == "sd") {
    return(sqrt(.pool_variance(stat^2, n, mean)))
  }
  weighted.mean(stat, n)
}

#' Pool variances exactly from per-cohort counts, means and variances
#'
#' @param variance A numeric vector of variances, one per cohort.
#' @param n A numeric vector of non-missing counts, one per cohort.
#' @param mean A numeric vector of means, one per cohort.
#' @return The variance of the pooled data.
#' @noRd
.pool_variance <- function(variance, n, mean) {
  pooled_mean <- weighted.mean(mean, n)
  (sum((n - 1) * variance) + sum(n * (mean - pooled_mean)^2)) / (sum(n) - 1)
}

#' Check that long summaries have the statistics needed for pooling
#'
#' @param long The long output of \code{ds.summarise}.
#' @return None. Throws an error if the summaries cannot be pooled.
#' @importFrom cli cli_abort
#' @noRd
.check_poolable <- function(long) {
  vars <- dplyr::summarise(long, has_n = "n" %in% .data$stat_name, has_mean = "mean" %in% .data$stat_name,
    needs_mean = any(c("sd", "var") %in% .data$stat_name), .by = "variable")
  if (!all(vars$has_n) || any(vars$needs_mean & !vars$has_mean)) {
    cli_abort(c(
      "Pooling needs an {.val n} statistic for every variable, and a {.val mean} for every {.val sd} or {.val var}.",
      "i" = "For example: {.code across(c(x, y), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd))}."
    ))
  }
}
