library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

mtcars_bad_group <- mtcars %>% group_by(qsec)

dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group,
    mtcars_bad_group = mtcars_bad_group
  )
)

dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$aggregateMethod("groupKeysDS", "groupKeysDS")
dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server="server_1",
  url="dslite.server",
  table = "mtcars",
  driver = "DSLiteDriver")

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = TRUE)

datashield.assign.table(
  conns = conns,
  table = "mtcars_group",
  symbol = "mtcars_group")

datashield.assign.table(
  conns = conns,
  table = "mtcars_bad_group",
  symbol = "mtcars_bad_group")

test_that("ds.group_keys correctly returns groups", {

  groups <- ds.group_keys("mtcars_group")

  expect_equal(
    groups[[1]],
    tibble(cyl = c(4, 6, 8))
  )

})

test_that("ds.group_keys returns error if too many groups", {
  expect_error(
    ds.group_keys("mtcars_bad_group")
    )
})
