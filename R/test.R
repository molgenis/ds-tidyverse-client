install.packages("DSLite")
remotes::install_github("datashield/dsBase", ref = "6.3.0")
remotes::install_github("https://github.com/datashield/dsDangerClient")
remotes::install_github("https://github.com/datashield/dsDanger")
remotes::install_github("timcadman/dsTidyverse")
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

## Works
list(LAB_TSC,LAB_TRIG)
list(starts_with("PM"),ends_with("ED"))
list(everything())
list(last_col())
list(group_cols())
list(starts_with("PM"))
list(ends_with("ED"))
list(contains("S_DIA"))

## Doesn't work
list(all_of(c("LAB_TSC", "LAB_TRIG"))) ## Won't work like this
list(any_of(c("LAB_TSC", "LAB_TRIG"))) ## Won't work like this

## Not tested
num_range()
where()

ds.colnames("CNSIM1")
ds.select(.data = "CNSIM1", newobj = "test", select_args = list(all_of(c("LAB_TSC", "LAB_TRIG"))))
ds.colnames("test")


select_str <- 'all_of(c("LAB_TSC", "LAB_TRIG"))'
select_expr <- rlang::parse_exprs( select_str )
mtcars %>% select( !!!select_expr )


## Main things:
# I think more aesthetically pleasing to pass a list rather than a vector
# Need to account for the below cases:

# : for selecting a range of consecutive variables.
#
# ! for taking the complement of a set of variables.
#
# & and | for selecting the intersection or the union of two sets of variables.
#
# c() for combining selections.

# Need to try and write some sharper regex

# Introduce checks to make sure column names won't interfere with the conversion

# Write lots and lots of unit tests to check the regex

# Make a module for the character conversion so I can use in other tidyverse functions

# Write what ever other DS checks are required and make them better


