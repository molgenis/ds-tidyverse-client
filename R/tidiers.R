#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @title Mark a DataSHIELD model result with its model class
#' @description Adds the class `ds_glm`, `ds_glmSLMA` or `ds_lmerSLMA` to the output of
#'   \code{dsBaseClient::ds.glm}, \code{dsBaseClient::ds.glmSLMA} or \code{dsBaseClient::ds.lmerSLMA}, so
#'   that \code{tidy()} and \code{glance()} can be used on it. An object that already has the class is
#'   returned unchanged.
#' @param x The object returned by the model function.
#' @param type The model function that returned `x`: one of `"glm"`, `"glmSLMA"` or `"lmerSLMA"`.
#' @return `x` with the class `ds_<type>`.
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' fit <- dsBaseClient::ds.glmSLMA("y ~ x", family = "gaussian", dataName = "D")
#' tidy(as_ds_model(fit, "glmSLMA"), conf.int = TRUE)
#' }
#' @export
as_ds_model <- function(x, type = c("glm", "glmSLMA", "lmerSLMA")) {
  type <- match.arg(type)
  model_class <- paste0("ds_", type)
  if (inherits(x, model_class)) {
    return(x)
  }
  has_results <- if (type == "glm") !is.null(x[["coefficients"]]) else length(.slma_studies(x)) > 0
  if (!has_results) {
    cli_abort("{.arg x} does not contain model results from {.fn ds.{type}}.")
  }
  class(x) <- model_class
  x
}

#' @title Tidy and glance methods for DataSHIELD models
#' @description \code{tidy()} returns one row per model term. For study-level meta-analysis (SLMA)
#'   results it returns one block of rows per cohort that returned estimates, then a block with
#'   `cohort = "pooled"` holding the pooled estimates, if \code{ds.glmSLMA} or \code{ds.lmerSLMA}
#'   pooled them. Mixed models also return each cohort's random-effect standard deviations and
#'   correlations, with `effect = "ran_pars"`.
#'
#'   \code{glance()} returns one row for \code{ds.glm} and one row per cohort for SLMA results. Cohorts
#'   that failed have missing values.
#'
#'   Statistics, p-values and confidence intervals that the servers do not return are computed with a
#'   normal approximation.
#' @param x A model result marked with \code{as_ds_model()}.
#' @param conf.int Logical: whether to add the columns `conf.low` and `conf.high`.
#' @param conf.level The confidence level of the interval.
#' @param pooling The pooled estimates to report: `"REML"`, `"ML"` or `"FE"`, as estimated by
#'   \code{metafor::rma} in \code{ds.glmSLMA} and \code{ds.lmerSLMA}.
#' @param ... Not used.
#' @return A data frame.
#' @name ds_model_tidiers
NULL

#' @rdname ds_model_tidiers
#' @export
tidy.ds_glm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  .add_conf_int(.coefficient_rows(x[["coefficients"]][, 1:4, drop = FALSE]), conf.int, conf.level)
}

#' @rdname ds_model_tidiers
#' @export
tidy.ds_glmSLMA <- function(x, conf.int = FALSE, conf.level = 0.95, pooling = c("REML", "ML", "FE"), ...) {
  studies <- .valid_studies(x)
  rows <- lapply(studies, function(study) .coefficient_rows(study[["coefficients"]]))
  pooled <- .pooled_coefficients(x, studies, match.arg(pooling))
  if (!is.null(pooled)) {
    rows$pooled <- .coefficient_rows(pooled)
  }
  .add_conf_int(.bind_cohorts(rows), conf.int, conf.level)
}

#' @rdname ds_model_tidiers
#' @export
tidy.ds_lmerSLMA <- function(x, conf.int = FALSE, conf.level = 0.95, pooling = c("REML", "ML", "FE"), ...) {
  studies <- .valid_studies(x)
  rows <- lapply(studies, function(study) {
    rbind(.fixed_effect_rows(study[["coefficients"]]), .random_effect_rows(study[["varcor"]]))
  })
  pooled <- .pooled_coefficients(x, studies, match.arg(pooling))
  if (!is.null(pooled)) {
    rows$pooled <- .fixed_effect_rows(pooled)
  }
  .add_conf_int(.bind_cohorts(rows), conf.int, conf.level)
}

#' @rdname ds_model_tidiers
#' @export
glance.ds_glm <- function(x, ...) {
  data.frame(
    deviance = x[["dev"]],
    df.residual = x[["df"]],
    nobs = x[["Nvalid"]],
    n.missing = x[["Nmissing"]],
    converged = TRUE
  )
}

#' @rdname ds_model_tidiers
#' @export
glance.ds_glmSLMA <- function(x, ...) {
  .glance_cohorts(x, function(study) {
    data.frame(
      null.deviance = study[["deviance.null"]],
      df.null = study[["df.null"]],
      AIC = study[["aic"]],
      deviance = study[["deviance.resid"]],
      df.residual = study[["df.resid"]],
      nobs = study[["Nvalid"]],
      n.missing = study[["Nmissing"]],
      converged = study[["converged"]]
    )
  })
}

#' @rdname ds_model_tidiers
#' @export
glance.ds_lmerSLMA <- function(x, ...) {
  .glance_cohorts(x, function(study) {
    log_lik <- study[["logLik"]]
    n_par <- attr(log_lik, "df")
    nobs <- study[["Nvalid"]]
    data.frame(
      nobs = nobs,
      sigma = study[["sigma"]],
      logLik = as.numeric(log_lik),
      AIC = -2 * as.numeric(log_lik) + 2 * n_par,
      BIC = -2 * as.numeric(log_lik) + log(nobs) * n_par,
      df.residual = nobs - n_par,
      n.missing = study[["Nmissing"]],
      converged = .is_converged(study[["optinfo"]][["conv"]])
    )
  })
}

#' Get the per-study summaries of an SLMA result, whichever shape it was returned in
#' @param x The output of ds.glmSLMA or ds.lmerSLMA.
#' @return A named list with one element per study.
#' @noRd
.slma_studies <- function(x) {
  summaries <- if (is.null(x[["output.summary"]])) x else x[["output.summary"]]
  unclass(summaries)[grep("^study[0-9]+$", names(summaries))]
}

#' Get the per-study summaries of the studies that returned estimates
#' @param x The output of ds.glmSLMA or ds.lmerSLMA.
#' @return A named list with one element per study that returned coefficients.
#' @noRd
.valid_studies <- function(x) {
  studies <- Filter(function(study) !is.null(study[["coefficients"]]), .slma_studies(x))
  if (length(studies) == 0) {
    cli_abort("No study returned model estimates.")
  }
  studies
}

#' Make one row per term from a coefficient matrix
#' @param coefficients Matrix with estimates, standard errors, test statistics and, optionally,
#'   p-values in its first four columns, and the terms as row names.
#' @return A data frame with the columns term, estimate, std.error, statistic and p.value.
#' @noRd
.coefficient_rows <- function(coefficients) {
  data.frame(
    term = rownames(coefficients),
    estimate = coefficients[, 1],
    std.error = coefficients[, 2],
    statistic = coefficients[, 3],
    p.value = if (ncol(coefficients) > 3) coefficients[, 4] else NA_real_,
    row.names = NULL
  )
}

#' Make a coefficient matrix from the pooled estimates of an SLMA result
#'
#' The pooled matrix loses its row names when study 1 failed or only one study is valid, so the terms
#' are taken from a valid study.
#' @param x The output of ds.glmSLMA or ds.lmerSLMA.
#' @param studies The valid studies of `x`.
#' @param pooling The pooling method: "REML", "ML" or "FE".
#' @return A matrix as taken by .coefficient_rows, or NULL if `x` has no pooled estimates.
#' @noRd
.pooled_coefficients <- function(x, studies, pooling) {
  pooled <- x[["SLMA.pooled.ests.matrix"]]
  if (is.null(pooled)) {
    return(NULL)
  }
  estimate <- pooled[, paste0("pooled.", pooling)]
  std_error <- pooled[, paste0("se.", pooling)]
  statistic <- estimate / std_error
  coefficients <- cbind(estimate, std_error, statistic, 2 * stats::pnorm(-abs(statistic)))
  rownames(coefficients) <- rownames(studies[[1]][["coefficients"]])
  coefficients
}

#' Make the fixed-effect rows of a mixed model in broom.mixed form
#' @param coefficients The fixed-effect coefficient matrix.
#' @return A data frame with the columns effect, group and those of .coefficient_rows.
#' @noRd
.fixed_effect_rows <- function(coefficients) {
  cbind(effect = "fixed", group = NA_character_, .coefficient_rows(coefficients))
}

#' Make the random-effect standard deviation and correlation rows of a mixed model in broom.mixed form
#' @param varcor The `varcor` element of a lme4 model summary.
#' @return A data frame with the same columns as .fixed_effect_rows.
#' @noRd
.random_effect_rows <- function(varcor) {
  rows <- lapply(names(varcor), function(group) .group_sd_cor(varcor[[group]], group))
  if (isTRUE(attr(varcor, "useSc"))) {
    rows <- c(rows, list(data.frame(group = "Residual", term = "sd__Observation", estimate = attr(varcor, "sc"))))
  }
  rows <- do.call(rbind, rows)
  data.frame(
    effect = "ran_pars", group = rows$group, term = rows$term, estimate = rows$estimate,
    std.error = NA_real_, statistic = NA_real_, p.value = NA_real_
  )
}

#' Get the standard deviations and correlations of one grouping factor's random effects
#' @param covariance The covariance matrix of the group, with the attributes `stddev` and `correlation`.
#' @param group The name of the grouping factor.
#' @return A data frame with the columns group, term and estimate.
#' @noRd
.group_sd_cor <- function(covariance, group) {
  sds <- attr(covariance, "stddev")
  correlations <- attr(covariance, "correlation")
  pairs <- which(lower.tri(correlations), arr.ind = TRUE)
  data.frame(
    group = group,
    term = c(paste0("sd__", names(sds)), sprintf("cor__%s.%s", names(sds)[pairs[, "col"]], names(sds)[pairs[, "row"]])),
    estimate = c(unname(sds), correlations[pairs]),
    row.names = NULL
  )
}

#' Stack per-cohort data frames, adding a cohort column
#' @param rows A named list of data frames, named by cohort.
#' @return A data frame with the cohort column first.
#' @noRd
.bind_cohorts <- function(rows) {
  cohorts <- rep(names(rows), vapply(rows, nrow, integer(1)))
  cbind(cohort = cohorts, do.call(rbind, unname(rows)))
}

#' Add Wald confidence intervals using the normal approximation
#' @param rows A data frame with the columns estimate and std.error.
#' @param conf.int Logical: whether to add the interval.
#' @param conf.level The confidence level.
#' @return `rows`, with the columns conf.low and conf.high if `conf.int` is TRUE.
#' @noRd
.add_conf_int <- function(rows, conf.int, conf.level) {
  if (!conf.int) {
    return(rows)
  }
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  rows$conf.low <- rows$estimate - z * rows$std.error
  rows$conf.high <- rows$estimate + z * rows$std.error
  rows
}

#' Make one glance row per cohort, with missing values for cohorts that failed
#' @param x The output of ds.glmSLMA or ds.lmerSLMA.
#' @param glance_study Function making a one-row data frame from a valid study summary.
#' @return A data frame with the cohort column first.
#' @noRd
.glance_cohorts <- function(x, glance_study) {
  studies <- .slma_studies(x)
  valid <- .valid_studies(x)
  rows <- do.call(rbind, lapply(valid, glance_study))
  rows <- rows[match(names(studies), names(valid)), , drop = FALSE]
  cbind(cohort = names(studies), rows, row.names = NULL)
}

#' Check whether lme4 reported convergence
#'
#' A singular fit is reported as a message without a code, so it does not count as non-convergence.
#' @param conv The `optinfo$conv` element of a lme4 model summary.
#' @return Logical.
#' @noRd
.is_converged <- function(conv) {
  conv[["opt"]] == 0 && is.null(conv[["lme4"]][["code"]])
}
