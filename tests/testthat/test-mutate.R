library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("mutateDS", "dsTidyverse::mutateDS")
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

test_that("ds.mutate correctly passes good argument", {
  ds.mutate(
    df.name = "mtcars",
    tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
    newobj = "new"
  )
   expected_cols <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "mpg_trans", "new_var")
  .check_cols_as_expected(expected_cols, "new")

  expected_mean <- ds.mean("new$mpg_trans")
  expect_equal(
    mean(expected_mean$Mean.by.Study[1]),
    6187.5)
})

test_that("ds.mutate fails with bad argument argument", {
  expect_error(
    ds.mutate(
      df.name = "mtcars",
      tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec, filterasd("asdasdf")),
      newobj = "new"
    )
  )

  expect_error(
    ds.mutate(
      df.name = "mtcars",
      tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
      newobj = "new",
      .keep = NULL
    )
  )

})

test_that("ds.mutate passes with different .keep argument", {
  ds.mutate(
    df.name = "mtcars",
    tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
    newobj = "new",
    .keep = "none",
    .before = NULL,
    .after = NULL
  )

  .check_cols_as_expected(c("mpg_trans", "new_var"), "new")
})

test_that("ds.mutate passes with different .before argument", {
  ds.mutate(
    df.name = "mtcars",
    tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
    newobj = "new",
    .keep = "all",
    .before = "disp",
    .after = NULL
  )

  .check_cols_as_expected(
    c("mpg", "cyl", "mpg_trans", "new_var", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
  "new")
})

test_that("ds.mutate passes with different .after argument", {
  ds.mutate(
    df.name = "mtcars",
    tidy_select = list(mpg_trans = cyl*1000, new_var = (hp-drat)/qsec),
    newobj = "new",
    .keep = "all",
    .before = NULL,
    .after = "qsec"
  )

    .check_cols_as_expected(
      c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "mpg_trans", "new_var", "vs", "am", "gear", "carb"),
       "new")
})

