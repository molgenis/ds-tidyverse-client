library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars <- mtcars %>% mutate(cat_var = factor(ifelse(mpg > 20, "high", "low")))
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("ifElseDS", "dsTidyverse::ifElseDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

.check_cols_as_expected <- function(expected, df) {
  observed <- ds.colnames(df)[[1]]
  expected <- expected
  expect_equal(observed, expected)
}

test_that("ds.if_else correctly passes argument with numeric condition and categorical outcome", {
  ds.if_else(
    condition = list(mtcars$mpg > 20),
    "high",
    "low",
    newobj = "test")

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("high", "low", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "character"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )
})

test_that("ds.if_else correctly passes argument with numeric condition and numerical outcome", {
  ds.if_else(
    condition = list(mtcars$mpg > 20),
    99,
    100,
    newobj = "test")

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("99", "100", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "numeric"
  )

  counts <- ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )

})

test_that("ds.if_else correctly passes argument with = ", {

  ds.if_else(
    condition = list(mtcars$vs == "0"),
    "no",
    "yes",
    newobj = "testcat")

  names <- names(ds.table("testcat")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    names,
    c("no", "yes", "NA")
  )

  expect_equal(
    ds.class("testcat")[[1]],
    "character"
  )

  counts <- ds.table("testcat")$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(
    as.numeric(counts),
    c(54, 42, 0)
  )
})
