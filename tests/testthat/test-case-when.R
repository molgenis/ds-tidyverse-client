library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars <- mtcars %>% mutate(cat_var = factor(ifelse(mpg > 20, "high", "low")))
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("case_whenDS", "dsTidyverse::case_whenDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

test_that("ds.case_when passes and numeric condition and numeric output", {
  ds.case_when(
    dynamic_dots = list(
      mtcars$mpg < 20 ~ "low",
      mtcars$mpg >= 20 & mtcars$mpg < 30 ~ "medium",
      mtcars$mpg >= 30 ~ "high"),
    newobj = "test",
    datasources = conns)

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "low", "medium", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "character"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(12, 54, 30, 0)
  )
})

test_that("ds.case_when correctly passes argument with numeric condition and numeric outcome", {
  ds.case_when(
    dynamic_dots = list(
      mtcars$mpg < 20 ~ 20,
      mtcars$mpg >= 20 & mtcars$mpg < 30 ~ 30,
      mtcars$mpg >= 30 ~ 40),
    newobj = "test",
    datasources = conns)

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("20", "30", "40", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "numeric"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(54, 30, 12, 0)
  )

})


test_that("ds.case_when correctly passes argument with categorical condition and categorical outcome", {
  ds.case_when(
    dynamic_dots = list(
      mtcars$gear == 2 ~ "low",
      mtcars$gear == 3 ~ "medium",
      mtcars$gear == 4 ~ "high",
      mtcars$gear == 5 ~ "very_high"),
    newobj = "test",
    datasources = conns)

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "medium", "very_high", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "character"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(36, 45, 15, 0)
  )

})

test_that("ds.case_when correctly passes .default argument", {
  ds.case_when(
    dynamic_dots = list(
      mtcars$gear == 2 ~ "low",
      mtcars$gear == 3 ~ "medium",
      mtcars$gear == 5 ~ "very_high"),
    .default = "something_missing",
    newobj = "test",
    datasources = conns)

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("medium", "something_missing", "very_high", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "character"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(45, 36, 15, 0)
  )

})
