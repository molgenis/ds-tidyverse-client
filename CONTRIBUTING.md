# Contributing to dsTidyverse
We encourage everyone to contribute to dsTidyverse. You can do so by opening your own pull request (PR) on the repositories and we will review it. 

Below we have a couple pointers for developers new to DataSHIELD packages, DsTidyverse specifically.

## Client and serverside package
DsTidyverse consists of two parts: [the server side](https://github.com/molgenis/ds-tidyverse) and the [the clientside](https://github.com/molgenis/ds-tidyverse-client). Code that has to run on the server where the data is stored, goes in the server side package and the functions called by the user, belong in the client side package. 

## Unittests
We have a very high test coverage and we aim to keep it that way. Therefor, if you're adding any functionality, please write unittests for them. Codecov will automatically check the coverage whenever you open your PR. It will also run all tests to ensure everything is still working the way it's supposed to. 

## Testing in Armadillo
The tests run using DsLite. To ensure functionality also works with armadillo, use the information in the [PR template](pull_request_template). Assign all data required for the test and then run the tests on the armadillo server. 

## Branchname
Please be aware that when adding server/clientside functionality, that the branchnames should match. Otherwise the automatic tests will fail because the serverside package won't get installed properly. 
