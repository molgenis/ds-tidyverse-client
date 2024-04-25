library(assertthat)

logindata.dslite.cnsim <- setupCNSIMTest()
dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
conns <- datashield.login(logindata.dslite.cnsim, assign=TRUE)

tryCatch(
  ds.select(
    .data = "CNSIM1",
    tidy_select = list(LAB_TSC, starts_with("LAB"),ends_with("ED")),
    newobj = "test"),
  error=function(e) {
    message('An Error Occurred')
    print(e)
  },
  #if a warning occurs, tell me the warning
  warning=function(w) {
    message('A Warning Occurred')
    print(w)
    return(NA)
  }
)

