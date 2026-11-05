read_fixture <- function(name) readRDS(test_path("fixtures", paste0(name, ".rds")))

glm_gaussian <- as_ds_model(read_fixture("glm_gaussian"), "glm")
glm_binomial <- as_ds_model(read_fixture("glm_binomial"), "glm")
slma_raw <- read_fixture("glmSLMA_full")
slma <- as_ds_model(slma_raw, "glmSLMA")
slma_no_metafor <- as_ds_model(read_fixture("glmSLMA_no_metafor"), "glmSLMA")
slma_failed <- as_ds_model(read_fixture("glmSLMA_study1_failed"), "glmSLMA")
lmer_raw <- read_fixture("lmerSLMA_full")
lmer <- as_ds_model(lmer_raw, "lmerSLMA")
lmer_failed <- as_ds_model(read_fixture("lmerSLMA_studies_failed"), "lmerSLMA")
glmer <- as_ds_model(read_fixture("glmerSLMA_full"), "glmerSLMA")
glmer_no_metafor <- as_ds_model(read_fixture("glmerSLMA_no_metafor"), "glmerSLMA")
glmer_failed <- as_ds_model(read_fixture("glmerSLMA_studies_failed"), "glmerSLMA")
z <- stats::qnorm(0.975)

test_that("as_ds_model adds the class once", {
  expect_s3_class(slma, "ds_glmSLMA")
  expect_equal(class(lmer), "ds_lmerSLMA")
  expect_identical(as_ds_model(slma, "glmSLMA"), slma)
  expect_equal(unclass(slma), unclass(slma_raw))
})

test_that("as_ds_model rejects outputs without model results", {
  expect_error(as_ds_model(NULL, "glm"), "does not contain model results")
  expect_error(as_ds_model(list(errorMessage = "ERROR: the only optimizer"), "lmerSLMA"), "does not contain")
  blocked <- list("EVERY STUDY HAS DATA", "Any values of 1", "Errors by study", y.vector.error = matrix(0))
  expect_error(as_ds_model(blocked, "glmSLMA"), "does not contain")
  messages <- list(is.object.created = "A data object", validity.check = "invalid", studyside.messages = list())
  expect_error(as_ds_model(messages, "glmSLMA"), "does not contain")
})

test_that("tidy.ds_glm returns the pooled coefficients", {
  out <- tidy(glm_gaussian)
  coefs <- glm_gaussian$coefficients
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("cohort", "term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(out$cohort, rep("pooled", 3))
  expect_equal(out$term, rownames(coefs))
  expect_equal(out$estimate, unname(coefs[, "Estimate"]))
  expect_equal(out$p.value, unname(coefs[, "p-value"]))
})

test_that("tidy.ds_glm confidence intervals match ds.glm's for every family", {
  out <- tidy(glm_binomial, conf.int = TRUE)
  expect_equal(out$conf.low, unname(glm_binomial$coefficients[, "low0.95CI.LP"]))
  expect_equal(out$conf.high, unname(glm_binomial$coefficients[, "high0.95CI.LP"]))
  expect_equal(tidy(glm_gaussian, conf.int = TRUE)$conf.low, unname(glm_gaussian$coefficients[, "low0.95CI"]))
})

test_that("tidy.ds_glm exponentiates estimates and intervals but not standard errors", {
  linear <- tidy(glm_binomial, conf.int = TRUE)
  out <- tidy(glm_binomial, conf.int = TRUE, exponentiate = TRUE)
  expect_equal(out$estimate, exp(linear$estimate))
  expect_equal(out$conf.low, exp(linear$conf.low))
  expect_equal(out$conf.high, exp(linear$conf.high))
  expect_equal(out$std.error, linear$std.error)
  expect_equal(out$estimate[-1], unname(glm_binomial$coefficients[-1, "P_OR"]))
})

test_that("tidy.ds_glm keeps the term of a one-term model", {
  intercept_only <- glm_gaussian
  intercept_only$coefficients <- glm_gaussian$coefficients[1, , drop = FALSE]
  expect_equal(tidy(intercept_only)$term, "(Intercept)")
})

test_that("glance.ds_glm returns one row", {
  out <- glance(glm_gaussian)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(names(out)[1], "cohort")
  expect_equal(out$cohort, "pooled")
  expect_equal(out$nobs, glm_gaussian$Nvalid)
  expect_equal(out$deviance, glm_gaussian$dev)
  expect_equal(out$df.residual, glm_gaussian$df)
})

test_that("tidy.ds_glmSLMA returns a block per cohort and pooled rows", {
  out <- tidy(slma)
  expect_equal(names(out), c("cohort", "term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(out$cohort, rep(c("study1", "study2", "study3", "pooled"), each = 3))
  study2 <- out[out$cohort == "study2", ]
  expect_equal(study2$estimate, unname(slma$output.summary$study2$coefficients[, 1]))
  expect_equal(study2$p.value, unname(slma$output.summary$study2$coefficients[, 4]))
})

test_that("tidy.ds_glmSLMA computes pooled statistics for the chosen pooling method", {
  pooled_matrix <- slma$SLMA.pooled.ests.matrix
  reml <- tidy(slma)[10:12, ]
  expect_equal(reml$estimate, unname(pooled_matrix[, "pooled.REML"]))
  expect_equal(reml$statistic, unname(pooled_matrix[, "pooled.REML"] / pooled_matrix[, "se.REML"]))
  expect_equal(reml$p.value, 2 * stats::pnorm(-abs(reml$statistic)))
  fe <- tidy(slma, pooling = "FE", conf.int = TRUE, conf.level = 0.9)[10:12, ]
  expect_equal(fe$std.error, unname(pooled_matrix[, "se.FE"]))
  expect_equal(fe$conf.high, fe$estimate + stats::qnorm(0.95) * fe$std.error)
})

test_that("tidy.ds_glmSLMA exponentiates cohort and pooled rows", {
  linear <- tidy(slma, conf.int = TRUE)
  out <- tidy(slma, conf.int = TRUE, exponentiate = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$estimate, exp(linear$estimate))
  expect_equal(out$conf.high, exp(linear$conf.high))
  expect_equal(out$std.error, linear$std.error)
})

test_that("tidy.ds_glmSLMA handles results without pooled estimates", {
  expect_equal(unique(tidy(slma_no_metafor)$cohort), c("study1", "study2", "study3"))
  structure_mismatch <- as_ds_model(list(output.summary = slma$output.summary), "glmSLMA")
  expect_equal(tidy(structure_mismatch), tidy(slma_no_metafor))
})

test_that("tidy.ds_glmSLMA leaves out failed studies and keeps pooled term names", {
  out <- tidy(slma_failed)
  expect_equal(unique(out$cohort), c("study2", "study3", "pooled"))
  expect_equal(out$term[out$cohort == "pooled"], c("(Intercept)", "GENDER1", "PM_BMI_CONTINUOUS"))
})

test_that("glance.ds_glmSLMA returns one row per cohort", {
  out <- glance(slma)
  expect_s3_class(out, "tbl_df")
  study1 <- slma$output.summary$study1
  expect_equal(out$cohort, c("study1", "study2", "study3"))
  expect_equal(out$AIC[1], study1$aic)
  expect_equal(out$null.deviance[1], study1$deviance.null)
  expect_equal(out$df.residual[1], study1$df.resid)
  expect_equal(out$nobs[1], study1$Nvalid)
  expect_true(all(out$converged))
})

test_that("glance.ds_glmSLMA gives failed cohorts missing values", {
  out <- glance(slma_failed)
  expect_equal(out$cohort, c("study1", "study2", "study3"))
  expect_true(all(is.na(out[1, -1])))
  expect_equal(out$nobs[2:3], c(400, 400))
})

test_that("tidy.ds_lmerSLMA returns fixed and random-effect rows in broom.mixed form", {
  out <- tidy(lmer, conf.int = TRUE)
  expect_equal(
    names(out),
    c("cohort", "effect", "group", "term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  )
  study1 <- out[out$cohort == "study1", ]
  varcor <- lmer$output.summary$study1$varcor
  expect_equal(study1$effect, c("fixed", "fixed", rep("ran_pars", 4)))
  expect_equal(study1$group, c(NA, NA, "clinic", "clinic", "clinic", "Residual"))
  expect_equal(study1$term[3:6], c("sd__(Intercept)", "sd__LAB_TSC", "cor__(Intercept).LAB_TSC", "sd__Observation"))
  expect_equal(study1$estimate[3:4], unname(attr(varcor$clinic, "stddev")))
  expect_equal(study1$estimate[5], attr(varcor$clinic, "correlation")[2, 1])
  expect_equal(study1$estimate[6], attr(varcor, "sc"))
  expect_true(all(is.na(study1$p.value)))
  expect_true(all(is.na(study1$conf.low[3:6])))
  expect_equal(study1$conf.low[1], study1$estimate[1] - z * study1$std.error[1])
})

test_that("tidy.ds_lmerSLMA pools fixed effects only", {
  pooled <- tidy(lmer, pooling = "ML")
  pooled <- pooled[pooled$cohort == "pooled", ]
  expect_equal(pooled$effect, c("fixed", "fixed"))
  expect_equal(pooled$estimate, unname(lmer$SLMA.pooled.ests.matrix[, "pooled.ML"]))
  expect_false(anyNA(pooled$p.value))
})

test_that("tidy.ds_lmerSLMA exponentiates fixed effects only", {
  linear <- tidy(lmer, conf.int = TRUE)
  out <- tidy(lmer, conf.int = TRUE, exponentiate = TRUE)
  fixed <- out$effect == "fixed"
  expect_equal(out$estimate[fixed], exp(linear$estimate[fixed]))
  expect_equal(out$conf.low[fixed], exp(linear$conf.low[fixed]))
  expect_equal(out$estimate[!fixed], linear$estimate[!fixed])
  expect_equal(out$std.error, linear$std.error)
})

test_that("tidy.ds_lmerSLMA handles failed studies and a single random intercept", {
  out <- tidy(lmer_failed)
  expect_equal(unique(out$cohort), c("study1", "pooled"))
  expect_equal(out$term[out$effect == "ran_pars"], c("sd__(Intercept)", "sd__Observation"))
  expect_equal(out$term[out$cohort == "pooled"], c("(Intercept)", "LAB_TSC", "GENDER1"))
})

test_that("glance.ds_lmerSLMA returns one row per cohort", {
  out <- glance(lmer)
  study1 <- lmer$output.summary$study1
  log_lik <- study1$logLik
  expect_equal(out$cohort, c("study1", "study2", "study3"))
  expect_equal(out$logLik[1], as.numeric(log_lik))
  expect_equal(out$AIC[1], -2 * as.numeric(log_lik) + 2 * attr(log_lik, "df"))
  expect_equal(out$sigma[1], study1$sigma)
  expect_equal(out$converged, c(TRUE, FALSE, FALSE))
})

test_that("glance.ds_lmerSLMA gives failed cohorts missing values", {
  out <- glance(lmer_failed)
  expect_equal(out$nobs, c(400, NA, NA))
  expect_equal(out$converged, c(TRUE, NA, NA))
})

test_that("tidy.ds_glmerSLMA returns cohort z-tests and no residual row for a binomial model", {
  out <- tidy(glmer)
  study1 <- out[out$cohort == "study1", ]
  coefs <- glmer$output.summary$study1$coefficients
  expect_equal(study1$effect, c("fixed", "fixed", "ran_pars"))
  expect_equal(study1$group, c(NA, NA, "clinic"))
  expect_equal(study1$term, c("(Intercept)", "LAB_TSC", "sd__(Intercept)"))
  expect_equal(study1$statistic[1:2], unname(coefs[, "z value"]))
  expect_equal(study1$p.value[1:2], unname(coefs[, "Pr(>|z|)"]))
  expect_equal(study1$estimate[3], unname(attr(glmer$output.summary$study1$varcor$clinic, "stddev")))
})

test_that("tidy.ds_glmerSLMA pools fixed effects and exponentiates them only", {
  out <- tidy(glmer, conf.int = TRUE, exponentiate = TRUE)
  linear <- tidy(glmer, conf.int = TRUE)
  pooled <- out[out$cohort == "pooled", ]
  expect_equal(pooled$estimate, exp(unname(glmer$SLMA.pooled.ests.matrix[, "pooled.REML"])))
  fixed <- out$effect == "fixed"
  expect_equal(out$conf.low[fixed], exp(linear$conf.low[fixed]))
  expect_equal(out$estimate[!fixed], linear$estimate[!fixed])
  expect_equal(unique(tidy(glmer_no_metafor)$cohort), c("study1", "study2", "study3"))
})

test_that("tidy.ds_glmerSLMA and glance.ds_glmerSLMA handle failed studies", {
  expect_equal(unique(tidy(glmer_failed)$cohort), c("study1", "pooled"))
  expect_equal(tidy(glmer_failed)$term[tidy(glmer_failed)$cohort == "pooled"], c("(Intercept)", "LAB_TSC", "GENDER1"))
  out <- glance(glmer_failed)
  expect_equal(out$nobs, c(400, NA, NA))
  expect_equal(out$converged, c(TRUE, NA, NA))
})

test_that("glance.ds_glmerSLMA returns one row per cohort", {
  out <- glance(glmer)
  log_lik <- glmer$output.summary$study2$logLik
  expect_s3_class(out, "tbl_df")
  expect_equal(out$cohort, c("study1", "study2", "study3"))
  expect_equal(out$BIC[2], -2 * as.numeric(log_lik) + log(400) * attr(log_lik, "df"))
  expect_true(all(out$converged))
})

test_that("modelsummary renders one and several ds.glm models", {
  skip_if_not_installed("modelsummary")
  one <- modelsummary::modelsummary(glm_gaussian, output = "data.frame")
  expect_true("(Intercept)" %in% one$term)
  several <- modelsummary::modelsummary(list(glm_gaussian, glm_binomial), output = "data.frame")
  expect_equal(names(several)[4:5], c("(1)", "(2)"))
})

test_that("modelsummary renders SLMA models with a cohort layout", {
  skip_if_not_installed("modelsummary")
  out <- modelsummary::modelsummary(
    list(a = slma, b = slma_failed),
    shape = term + cohort ~ model, gof_map = NA, output = "data.frame"
  )
  expect_setequal(out$cohort, c("study1", "study2", "study3", "pooled"))
  mixed <- modelsummary::modelsummary(
    lmer,
    shape = term + group + cohort ~ model, gof_map = NA, output = "data.frame"
  )
  expect_true("sd__Observation" %in% mixed$term)
})

test_that("modelsummary renders a glmer SLMA model", {
  skip_if_not_installed("modelsummary")
  out <- modelsummary::modelsummary(
    list(lmer = lmer, glmer = glmer),
    shape = term + group + cohort ~ model, gof_map = NA, output = "data.frame"
  )
  expect_true(all(c("sd__(Intercept)", "LAB_TSC") %in% out$term))
  expect_setequal(out$cohort, c("study1", "study2", "study3", "pooled"))
})

test_that("modelsummary renders a ds.glm and an SLMA model together", {
  skip_if_not_installed("modelsummary")
  out <- modelsummary::modelsummary(
    list(glm = glm_gaussian, slma = slma),
    shape = term + cohort ~ model, gof_map = NA, output = "data.frame"
  )
  pooled_intercept <- out[out$term == "(Intercept)" & out$cohort == "pooled" & out$statistic == "estimate", ]
  expect_equal(pooled_intercept$glm, sprintf("%.3f", glm_gaussian$coefficients[1, 1]))
  expect_equal(pooled_intercept$slma, sprintf("%.3f", slma$SLMA.pooled.ests.matrix[1, "pooled.REML"]))
  expect_equal(out$glm[out$cohort == "study1"], rep("", 6))
})
