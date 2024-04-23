# install.packages("DSLite")
# remotes::install_github("datashield/dsBase", ref = "6.3.0")
# remotes::install_github("https://github.com/datashield/dsDangerClient")
# remotes::install_github("https://github.com/datashield/dsDanger")
# remotes::install_github("timcadman/dsTidyverse")
library(DSLite)
library(dsBaseClient)
library(dsDangerClient)
library(DSI)

## ---- Setup dsLite server ------------------------------------------------------------------------
logindata.dslite.cnsim <- setupCNSIMTest()
dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger", "dplyr", "purrr")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$assignMethod("map", "map")

## ---- Login --------------------------------------------------------------------------------------
conns <- datashield.login(logindata.dslite.cnsim, assign=TRUE)

## Not tested
# num_range()

ds.colnames("CNSIM1")

ds.select(
  .data = "CNSIM1",
  newobj = "test",
  select_args = list(LAB_TSC, LAB_TRIG,starts_with("a"), all_of(c("asd", "sdf", "dfg")))
)

  ,
  ds.colnames("test")
  )


select_str <- 'all_of(c("LAB_TSC", "LAB_TRIG"))'
select_expr <- rlang::parse_exprs( select_str )
mtcars %>% select( !!!select_expr )




# Introduce checks to make sure column names won't interfere with the conversion

# Write what ever other DS checks are required and make them better


