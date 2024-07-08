library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("if_elseDS", "dsTidyverse::if_elseDS")
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
    1000,
    newobj = "test")

  nqmes <- names(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("99", "1000", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "numeric"
  )

  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )

})

test_that("ds.if_else correctly passes argument with categorical condition and numerical outcome", {

  ds.if_else(
    condition = list(mtcars$mpg > 20),
    "high",
    "asdasdasd",
    newobj = "newdf")



  ds.table("newdf")

  ds.if_else(
    condition = list(newdf == "high"),
    99,
    100,
    newobj = "test_2")

  ds.ls()

  ds.class("test_2")

  nqmes <- names(ds.table("test_2")$output.list$TABLES.COMBINED_all.sources_counts)

  expect_equal(
    nqmes,
    c("99", "1000", "NA")
  )

  expect_equal(
    ds.class("test")[[1]],
    "numeric"
  )

  expect_equal(
    as.numeric(counts),
    c(42, 54, 0)
  )

})


