require(DSLite)
require(DSI)
require(dplyr)
require(dsTidyverse)
require(dsBaseClient)

cohorts <- list(mtcars_1 = mtcars[1:11, ], mtcars_2 = mtcars[12:22, ], mtcars_3 = mtcars[23:32, ])
login_data <- .prepare_dslite(tables = c(cohorts, list(mtcars = mtcars)))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns["server_1"], "df", "mtcars_1")
datashield.assign.table(conns["server_2"], "df", "mtcars_2")
datashield.assign.table(conns["server_3"], "df", "mtcars_3")

summarise_by_cohort <- function(...) {
  cohorts |>
    setNames(c("server_1", "server_2", "server_3")) |>
    purrr::map(\(x) summarise(x, ..., .by = am)) |>
    purrr::list_rbind(names_to = "cohort") |>
    as_tibble()
}

test_that("ds.summarise returns one row per cohort and group in dplyr's shape", {
  observed <- ds.summarise("df", n = n(), mpg_mean = mean(mpg), .by = "am", datasources = conns)
  expect_equal(observed, summarise_by_cohort(n = n(), mpg_mean = mean(mpg)))
})

test_that("ds.summarise works with across and a list of functions", {
  observed <- ds.summarise(
    "df", across(c(mpg, hp), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd)),
    .by = "am", datasources = conns
  )
  expect_equal(observed, summarise_by_cohort(across(c(mpg, hp), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd))))
})

test_that("ds.summarise works without groups", {
  observed <- ds.summarise("df", n = n(), datasources = conns)
  expect_equal(observed, tibble(cohort = c("server_1", "server_2", "server_3"), n = c(11L, 11L, 10L)))
})

test_that("ds.summarise returns one row per cohort, group, variable and statistic when .shape is long", {
  observed <- ds.summarise(
    "df", n = n(), across(c(mpg, hp), list(mean = mean, p25 = ~ quantile(.x, 0.25))),
    .by = "am", .shape = "long", datasources = conns
  )
  expect_equal(names(observed), c("cohort", "am", "variable", "stat_name", "stat"))
  expect_equal(nrow(observed), 3 * 2 * 5)
  expect_equal(unique(observed$variable), c(NA, "mpg", "hp"))
  expect_equal(unique(observed$stat_name), c("n", "mean", "p25"))
})

test_that("ds.summarise pools n, mean and sd exactly", {
  observed <- ds.summarise(
    "df", across(c(mpg, hp), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd, var = var)),
    .by = "am", .shape = "long", .pooled = TRUE, datasources = conns
  ) |>
    filter(cohort == "pooled") |>
    select(-cohort) |>
    arrange(am, variable, stat_name)
  expected <- mtcars |>
    summarise(across(c(mpg, hp), list(n = ~ sum(!is.na(.x)), mean = mean, sd = sd, var = var)), .by = am) |>
    tidyr::pivot_longer(-am, names_to = c("variable", "stat_name"), names_sep = "_", values_to = "stat") |>
    arrange(am, variable, stat_name)
  expect_equal(observed, expected)
})

test_that("ds.summarise pools quantiles as an n-weighted average", {
  observed <- ds.summarise(
    "df", across(mpg, list(n = ~ sum(!is.na(.x)), median = median, p25 = ~ quantile(.x, 0.25))),
    .by = "am", .pooled = TRUE, datasources = conns
  ) |>
    filter(cohort == "pooled", am == 0)
  by_cohort <- summarise_by_cohort(n = n(), median = median(mpg), p25 = quantile(mpg, 0.25)) |> filter(am == 0)
  expect_equal(observed$mpg_median, weighted.mean(by_cohort$median, by_cohort$n))
  expect_equal(observed$mpg_p25, weighted.mean(by_cohort$p25, by_cohort$n), ignore_attr = TRUE)
})

test_that("ds.summarise adds pooled rows in the wide shape", {
  observed <- ds.summarise(
    "df", across(mpg, list(n = ~ sum(!is.na(.x)), mean = mean)),
    .by = "am", .pooled = TRUE, datasources = conns
  )
  expect_equal(names(observed), c("cohort", "am", "mpg_n", "mpg_mean"))
  expect_equal(observed$cohort, rep(c("server_1", "server_2", "server_3", "pooled"), each = 2))
})

test_that("ds.summarise pooling requires an n statistic for each variable", {
  expect_error(
    ds.summarise("df", across(mpg, list(mean = mean)), .by = "am", .pooled = TRUE, datasources = conns),
    "Pooling needs an"
  )
  expect_error(
    ds.summarise("df", across(mpg, list(n = ~ sum(!is.na(.x)), sd = sd)), .by = "am", .pooled = TRUE, datasources = conns),
    "Pooling needs an"
  )
})

test_that("ds.summarise long shape names columns that do not end in a recognised statistic", {
  expect_error(
    ds.summarise("df", n = n(), avg = mean(mpg), across(hp, list(max_ish = median)), .by = "am",
      .shape = "long", datasources = conns),
    "\"avg\" and \"hp_max_ish\" do not"
  )
  expect_error(
    ds.summarise("df", n = n(), mpg_mean = mean(mpg), mpg_q = median(mpg), .by = "am",
      .shape = "long", datasources = conns),
    "\"mpg_q\" does not"
  )
})

test_that("ds.summarise pooling rejects statistics it does not recognise", {
  expect_error(
    ds.summarise("df", n = n(), avg = mean(mpg), .by = "am", .pooled = TRUE, datasources = conns),
    "must end in a recognised statistic"
  )
})

test_that("ds.summarise wide shape keeps any column names", {
  observed <- ds.summarise("df", n = n(), avg = mean(mpg), .by = "am", datasources = conns)
  expect_equal(names(observed), c("cohort", "am", "n", "avg"))
})

test_that("ds.summarise returns server errors for disclosive requests", {
  expect_error(ds.summarise("df", m = max(mpg), datasources = conns), "DataSHIELD errors")
  expect_match(unlist(datashield.errors()), "not permitted")
  expect_error(ds.summarise("df", n = n(), .by = "carb", datasources = conns), "DataSHIELD errors")
  expect_match(unlist(datashield.errors()), "fewer than nfilter.tab")
})

test_that("ds.summarise checks its arguments", {
  expect_error(ds.summarise(df, n = n(), datasources = conns))
  expect_error(ds.summarise("df", n = n(), .shape = "tall", datasources = conns), "should be one of")
})

test_that("ds.count returns counts per cohort", {
  observed <- ds.count("df", am, datasources = conns)
  expected <- cohorts |>
    setNames(c("server_1", "server_2", "server_3")) |>
    purrr::map(\(x) count(x, am)) |>
    purrr::list_rbind(names_to = "cohort") |>
    as_tibble()
  expect_equal(observed, expected)
})

test_that("ds.count adds pooled counts", {
  observed <- ds.count("df", am, .pooled = TRUE, datasources = conns) |> filter(cohort == "pooled")
  expect_equal(observed$n, count(mtcars, am)$n)
})

test_that("ds.count returns server errors for small counts", {
  expect_error(ds.count("df", carb, datasources = conns), "DataSHIELD errors")
  expect_match(unlist(datashield.errors()), "between 1 and nfilter.tab")
})
