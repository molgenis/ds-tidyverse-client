#
# Molgenis' Armadillo - Upload Testing Datasets
#
# Run from this directory. Uploads the tables in .test_tables() that are not on the server yet;
# existing tables are left as they are.
#

library(MolgenisArmadillo)
source("../helpers.R")

armadillo.login_basic(armadillo = "http://localhost:8080", username = "admin", password = "admin")

if (!"tidyverse" %in% armadillo.list_projects()) {
  armadillo.create_project("tidyverse")
}

tables <- .test_tables()
existing <- armadillo.list_tables("tidyverse")
for (name in names(tables)[!.test_table(names(tables)) %in% existing]) {
  armadillo.upload_table("tidyverse", "test", tables[[name]], name)
}

print(armadillo.list_tables("tidyverse"))
