require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

make_cohort <- function(n, seed) {
  set.seed(seed)
  tibble(
    sex = factor(rep(c("F", "M"), length.out = n)),
    age = round(rnorm(n, 30, 5), 1),
    smk = factor(rep(c("Never", "Former", "Current"), length.out = n), levels = c("Never", "Former", "Current")),
    visit = as.Date("2024-01-01")
  )
}
cohorts <- list(c1 = make_cohort(60, 1), c2 = make_cohort(50, 2), c3 = make_cohort(40, 3))
cohorts$c1$age[1:4] <- NA
cohorts$c2$smk[1:6] <- NA
mixed <- mutate(cohorts$c1, age = as.character(age))
sparse <- mutate(cohorts$c1, rare = factor(c("a", rep("b", 59))), sparse_age = c(30, 31, rep(NA, 58)))
pooled_data <- bind_rows(cohorts)

login_data <- .prepare_dslite(tables = c(cohorts, list(mixed = mixed, sparse = sparse)))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns["server_1"], "df", "c1")
datashield.assign.table(conns["server_2"], "df", "c2")
datashield.assign.table(conns["server_3"], "df", "c3")
datashield.assign.table(conns["server_1"], "mixed_df", "mixed")
datashield.assign.table(conns["server_2"], "mixed_df", "c2")
datashield.assign.table(conns["server_3"], "mixed_df", "c3")
datashield.assign.table(conns, "sparse_df", "sparse")

mean_sd <- list(gtsummary::all_continuous() ~ "{mean} ({sd})", gtsummary::all_categorical() ~ "{n} ({p}%)")
local_summary <- function(...) {
  gtsummary::tbl_summary(
    pooled_data, include = c(age, smk), statistic = mean_sd, digits = gtsummary::all_continuous() ~ 1, ...
  )
}

test_that("ds.table_one returns a gtsummary table", {
  tbl <- ds.table_one("df", c("age", "smk"), .by = "sex", datasources = conns)
  expect_s3_class(tbl, "gtsummary")
})

test_that("ds.table_one matches tbl_summary for exactly pooled statistics, with an overall column", {
  tbl <- ds.table_one("df", c("age", "smk"), .by = "sex", statistic = mean_sd, overall = TRUE, datasources = conns)
  expected <- gtsummary::add_overall(local_summary(by = sex))
  expect_equal(gtsummary::as_tibble(tbl, col_labels = FALSE), gtsummary::as_tibble(expected, col_labels = FALSE))
})

test_that("ds.table_one shows group N in the headers", {
  tbl <- ds.table_one("df", c("age", "smk"), .by = "sex", overall = TRUE, datasources = conns)
  expect_equal(
    names(gtsummary::as_tibble(tbl)),
    c("**Characteristic**", "**Overall**  \nN = 150", "**F**  \nN = 75", "**M**  \nN = 75")
  )
})

test_that("ds.table_one works without .by", {
  tbl <- ds.table_one("df", c("age", "smk"), statistic = mean_sd, datasources = conns)
  expect_equal(gtsummary::as_tibble(tbl, col_labels = FALSE), gtsummary::as_tibble(local_summary(), col_labels = FALSE))
})

test_that("ds.table_one adds Unknown rows for missing values unless missing is 'no'", {
  with_missing <- gtsummary::as_tibble(ds.table_one("df", c("age", "smk"), .by = "sex", datasources = conns), col_labels = FALSE)
  expect_equal(with_missing$stat_1[with_missing$label == "Unknown"], c("2", "3"))
  no_missing <- gtsummary::as_tibble(ds.table_one("df", c("age", "smk"), .by = "sex", missing = "no", datasources = conns), col_labels = FALSE)
  expect_false("Unknown" %in% no_missing$label)
})

test_that("ds.table_one uses labels", {
  tbl <- ds.table_one("df", c("age", "smk"), labels = list(age ~ "Age (years)"), datasources = conns)
  expect_equal(gtsummary::as_tibble(tbl, col_labels = FALSE)$label[1], "Age (years)")
})

test_that("ds.table_one notes that pooled quantiles are approximate only when they are shown", {
  with_quantiles <- ds.table_one("df", c("age", "smk"), datasources = conns)
  expect_match(with_quantiles$table_styling$source_note$source_note, "approximate")
  without_quantiles <- ds.table_one("df", c("age", "smk"), statistic = mean_sd, datasources = conns)
  expect_equal(nrow(without_quantiles$table_styling$source_note), 0)
})

test_that("ds.table_one rejects variables of unsupported or inconsistent class", {
  expect_error(ds.table_one("df", "visit", datasources = conns), "must have one class across cohorts")
  expect_error(ds.table_one("mixed_df", "age", datasources = conns), "must have one class across cohorts")
})

test_that("ds.table_one checks its arguments", {
  expect_error(ds.table_one("df", age, datasources = conns))
  expect_error(ds.table_one("df", "age", .by = c("sex", "smk"), datasources = conns))
})

test_that("ds.table_one shows the server message when a count is blocked", {
  expect_error(
    ds.table_one("sparse_df", "rare", datasources = conns),
    "server_1: At least one count is between 1 and nfilter.tab"
  )
})

test_that("ds.table_one shows the server message when a summary is blocked", {
  expect_error(
    ds.table_one("sparse_df", "sparse_age", datasources = conns),
    "server_2: At least one variable has between 1 and nfilter.tab"
  )
})
