#' @title Create a Table 1 of descriptive statistics
#' @description Summarises server-side variables with \code{ds.summarise} and \code{ds.count},
#' pools them across cohorts and renders them with \code{gtsummary::tbl_ard_summary}.
#' @details Numeric and integer variables are summarised as continuous, with N, mean, SD, median,
#' Q1 and Q3 available to \code{statistic}. Factor, character and logical variables are summarised
#' as categorical, with n, N and p available. Pooled medians and quartiles are n-weighted averages
#' of the cohort values, as in \code{dsBaseClient::ds.quantileMean}, so they are approximate.
#' Rows with a missing \code{.by} value are left out of the group columns but included in the
#' overall column.
#' @param df Character specifying a server-side data frame or tibble.
#' @param vars Character vector of the variables to summarise.
#' @param .by Optionally, the name of one categorical variable to give a column per level.
#' @param labels Passed to the \code{label} argument of \code{gtsummary::tbl_ard_summary}.
#' @param statistic Passed to \code{gtsummary::tbl_ard_summary}.
#' @param overall Logical; if \code{TRUE} and \code{.by} is given, add an overall column.
#' @param missing Passed to \code{gtsummary::tbl_ard_summary}: whether to show a row of missing
#' counts, labelled "Unknown", for each variable.
#' @param datasources DataSHIELD connections object.
#' @return A \code{gtsummary} table, which works with \code{as_gt()}, \code{as_flex_table()} and the
#' \code{modify_*()} functions.
#' @importFrom DSI datashield.aggregate
#' @importFrom rlang syms
#' @examples
#' \dontrun{
#' ds.table_one(
#'   "df",
#'   vars = c("age", "smoking"),
#'   .by = "sex",
#'   labels = list(age ~ "Maternal age (years)"),
#'   overall = TRUE,
#'   datasources = conns
#' )
#' }
#' @export
ds.table_one <- function(df = NULL, vars = NULL, .by = NULL, labels = NULL,
                         statistic = list(
                           gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
                           gtsummary::all_categorical() ~ "{n} ({p}%)"
                         ),
                         overall = FALSE, missing = c("ifany", "no", "always"), datasources = NULL) {
  missing <- match.arg(missing)
  datasources <- .set_datasources(datasources)
  .check_tidy_args(df, NULL, check_obj = FALSE)
  assertthat::assert_that(is.character(vars), is.null(.by) || assertthat::is.string(.by))
  types <- .get_variable_types(df, vars, datasources)
  by_counts <- .pooled_rows(ds.count(df, !!!syms(.by), .pooled = TRUE, datasources = datasources))
  ard <- cards::bind_ard(.summarise_table_one(df, types, .by, datasources), .total_n_ard(by_counts))
  if (!is.null(.by)) {
    by_ard <- dplyr::filter(.categorical_ard(by_counts, .by, NULL), .data$context == "categorical")
    ard <- cards::bind_ard(ard, .as_card(by_ard, NULL))
  }
  if (!is.null(.by) && overall) {
    ard <- cards::bind_ard(ard, .summarise_table_one(df, types, NULL, datasources))
  }
  tbl <- gtsummary::tbl_ard_summary(
    ard, by = dplyr::all_of(.by), statistic = statistic, label = labels, missing = missing,
    overall = overall && !is.null(.by)
  )
  tbl <- gtsummary::modify_header(tbl, gtsummary::all_stat_cols() ~ "**{level}**  \nN = {n}")
  .add_quantile_note(tbl, length(datasources))
}

#' Get whether each variable is continuous or categorical from its server-side class
#'
#' @param df Character specifying a server-side data frame.
#' @param vars Character vector of variable names.
#' @param datasources DataSHIELD connections object.
#' @return A named character vector of "continuous" or "categorical".
#' @importFrom cli cli_abort
#' @noRd
.get_variable_types <- function(df, vars, datasources) {
  types <- vapply(vars, function(var) {
    classes <- unique(unlist(lapply(datashield.aggregate(datasources, call("classDS", paste0(df, "$", var))), `[`, 1)))
    if (length(classes) == 1 && classes %in% c("numeric", "integer")) {
      return("continuous")
    }
    if (length(classes) == 1 && classes %in% c("factor", "character", "logical")) {
      return("categorical")
    }
    cli_abort("{.var {var}} must have one class across cohorts, numeric, integer, factor, character or logical, not {.val {classes}}.")
  }, character(1))
  return(types)
}

#' Keep the pooled rows of ds.summarise or ds.count output
#'
#' @param out Output of \code{ds.summarise} or \code{ds.count} with \code{.pooled = TRUE}.
#' @return \code{out} without the per-cohort rows or the \code{cohort} column.
#' @importFrom rlang .data
#' @noRd
.pooled_rows <- function(out) {
  dplyr::select(dplyr::filter(out, .data$cohort == "pooled"), -"cohort")
}

#' Summarise variables by group and convert them to an ARD
#'
#' @param df Character specifying a server-side data frame.
#' @param types Output of \code{.get_variable_types}.
#' @param .by The name of the grouping variable, or NULL.
#' @param datasources DataSHIELD connections object.
#' @return A cards ARD of the pooled statistics.
#' @noRd
.summarise_table_one <- function(df, types, .by, datasources) {
  continuous <- names(types)[types == "continuous"]
  categorical <- names(types)[types == "categorical"]
  rows <- purrr::map(categorical, function(var) {
    counts <- .pooled_rows(ds.count(df, !!!syms(c(.by, var)), .pooled = TRUE, datasources = datasources))
    .categorical_ard(counts, var, .by)
  })
  if (length(continuous) > 0) {
    rows <- c(list(.continuous_ard(.summarise_continuous(df, continuous, .by, datasources), .by)), rows)
  }
  .as_card(dplyr::bind_rows(rows), .by)
}

#' Get pooled N, mean, SD, median and quartiles of continuous variables
#'
#' @param df Character specifying a server-side data frame.
#' @param vars Character vector of continuous variables.
#' @param .by The name of the grouping variable, or NULL.
#' @param datasources DataSHIELD connections object.
#' @return The pooled rows of \code{ds.summarise(.shape = "long")}, with a group-level \code{n}.
#' @importFrom dplyr across n
#' @noRd
.summarise_continuous <- function(df, vars, .by, datasources) {
  out <- ds.summarise(
    df,
    n = n(),
    across(c(!!!syms(vars)), list(
      n = ~ sum(!is.na(.x)), mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE), p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
      p75 = ~ quantile(.x, 0.75, na.rm = TRUE)
    )),
    .by = .by, .shape = "long", .pooled = TRUE, datasources = datasources
  )
  .pooled_rows(out)
}

#' Convert pooled continuous summaries to ARD rows
#'
#' @param long Output of \code{.summarise_continuous}.
#' @param .by The name of the grouping variable, or NULL.
#' @return A tibble of continuous and missing-value statistics.
#' @noRd
.continuous_ard <- function(long, .by) {
  stats <- long |>
    dplyr::mutate(N_obs = .data$stat[is.na(.data$variable)], .by = dplyr::all_of(.by)) |>
    dplyr::filter(!is.na(.data$variable)) |>
    dplyr::mutate(stat_name = dplyr::if_else(.data$stat_name == "n", "N", .data$stat_name))
  observed <- stats |>
    dplyr::filter(.data$stat_name == "N") |>
    dplyr::select(dplyr::all_of(.by), "variable", "N_obs", N_nonmiss = "stat")
  dplyr::bind_rows(
    dplyr::mutate(dplyr::select(stats, -"N_obs"), context = "continuous"),
    .missing_ard(observed)
  )
}

#' Convert pooled counts of one categorical variable to ARD rows
#'
#' @param counts Pooled output of \code{ds.count}, with columns for the groups, the variable and
#' \code{n}.
#' @param var_name The name of the categorical variable.
#' @param .by The name of the grouping variable, or NULL.
#' @return A tibble of categorical and missing-value statistics.
#' @noRd
.categorical_ard <- function(counts, var_name, .by) {
  counts <- counts |>
    dplyr::rename(level = dplyr::all_of(var_name)) |>
    dplyr::mutate(variable = var_name, level = as.character(.data$level))
  observed <- dplyr::summarise(
    counts,
    N_obs = sum(.data$n), N_nonmiss = sum(.data$n[!is.na(.data$level)]),
    .by = dplyr::all_of(c(.by, "variable"))
  )
  tabulated <- counts |>
    dplyr::filter(!is.na(.data$level)) |>
    dplyr::mutate(N = sum(.data$n), p = .data$n / .data$N, .by = dplyr::all_of(.by)) |>
    tidyr::pivot_longer(c("n", "N", "p"), names_to = "stat_name", values_to = "stat") |>
    dplyr::mutate(context = "categorical", variable_level = as.list(.data$level)) |>
    dplyr::select(-"level")
  dplyr::bind_rows(tabulated, .missing_ard(observed))
}

#' Calculate missing-value statistics as ARD rows
#'
#' @param observed A tibble with the groups, \code{variable}, \code{N_obs} (rows) and
#' \code{N_nonmiss} (non-missing values).
#' @return A tibble of the statistics of \code{cards::ard_missing}.
#' @noRd
.missing_ard <- function(observed) {
  observed |>
    dplyr::mutate(
      N_miss = .data$N_obs - .data$N_nonmiss,
      p_miss = .data$N_miss / .data$N_obs,
      p_nonmiss = .data$N_nonmiss / .data$N_obs
    ) |>
    tidyr::pivot_longer(
      c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"),
      names_to = "stat_name", values_to = "stat"
    ) |>
    dplyr::mutate(context = "missing")
}

#' Create the ARD row giving the total number of rows
#'
#' @param by_counts Pooled output of \code{ds.count}, with a column \code{n}.
#' @return A cards ARD with one row.
#' @noRd
.total_n_ard <- function(by_counts) {
  row <- dplyr::tibble(variable = "..ard_total_n..", context = "total_n", stat_name = "N", stat = sum(by_counts$n))
  .as_card(row, NULL)
}

#' Convert ARD rows to a cards ARD, with labels and formats for each statistic
#'
#' @param rows A tibble with columns \code{variable}, \code{context}, \code{stat_name},
#' \code{stat}, optionally \code{variable_level}, and a column named \code{.by} if given.
#' @param .by The name of the grouping variable, or NULL. Rows where it is missing are dropped.
#' @return A cards ARD.
#' @noRd
.as_card <- function(rows, .by) {
  stat_labels <- c(
    N = "N", mean = "Mean", sd = "SD", median = "Median", p25 = "Q1", p75 = "Q3", n = "n", p = "%",
    N_obs = "Vector Length", N_miss = "N Missing", N_nonmiss = "N Non-missing",
    p_miss = "% Missing", p_nonmiss = "% Non-missing"
  )
  if (!"variable_level" %in% names(rows)) {
    rows$variable_level <- list(NULL)
  }
  if (!is.null(.by)) {
    rows <- rows |>
      dplyr::filter(!is.na(.data[[.by]])) |>
      dplyr::mutate(group1 = .by, group1_level = as.list(as.character(.data[[.by]])))
  }
  rows |>
    dplyr::mutate(
      stat_label = unname(stat_labels[.data$stat_name]),
      stat = as.list(.data$stat),
      fmt_fun = lapply(.data$stat_name, .format_function),
      warning = list(NULL),
      error = list(NULL)
    ) |>
    dplyr::select(
      dplyr::any_of(c("group1", "group1_level")), "variable", "variable_level", "context", "stat_name",
      "stat_label", "stat", "fmt_fun", "warning", "error"
    ) |>
    cards::as_card()
}

#' Get the format of a statistic
#'
#' @param stat_name The name of the statistic.
#' @return 0 decimals for counts, a percentage format for proportions, else 1 decimal.
#' @noRd
.format_function <- function(stat_name) {
  if (stat_name %in% c("n", "N", "N_obs", "N_miss", "N_nonmiss")) {
    return(0L)
  }
  if (stat_name %in% c("p", "p_miss", "p_nonmiss")) {
    return(gtsummary::label_style_percent())
  }
  1L
}

#' Add a source note that pooled quantiles are approximate
#'
#' @param tbl A gtsummary table.
#' @param n_cohorts The number of cohorts.
#' @return \code{tbl}, with a source note if it shows quantiles pooled from more than one cohort.
#' @noRd
.add_quantile_note <- function(tbl, n_cohorts) {
  shows_quantiles <- any(grepl("\\{(median|p25|p75)\\}", unlist(tbl$inputs$statistic)))
  if (n_cohorts > 1 && shows_quantiles) {
    tbl <- gtsummary::modify_source_note(
      tbl,
      "Pooled medians and quartiles are n-weighted averages of cohort values and are approximate."
    )
  }
  return(tbl)
}
