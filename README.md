# dsTidyverseClient
<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dsTidyverseClient)](https://CRAN.R-project.org/package=dsTidyverseClient)
[![CircleCI](https://circleci.com/gh/molgenis/ds-tidyverse-client.svg?style=shield)](https://app.circleci.com/pipelines/github/molgenis/ds-tidyverse-client)
[![codecov](https://codecov.io/gh/molgenis/ds-tidyverse-client/branch/master/graph/badge.svg?token=ITPMERAWYI)](https://app.codecov.io/gh/molgenis/ds-tidyverse-client)
<!-- badges: end -->

This is the DataSHIELD implementation of selected [Tidyverse](https://www.tidyverse.org/) functions. 
Currently most of these functions are from dplyr, however in the future functions from other
Tidyverse packages can be included.

## Installation
- Install dsTidyverse on your armadillo or opal server.
- Whitelist dsTidyverse if required
- Install dsTidyverseClient on your local machine

## Functions/Features
### From the `dplyr` package:
- **`select`**: Choose columns from a data frame.
- **`rename`**: Rename columns in a data frame.
- **`mutate`**: Create or modify columns.
- **`if_else`**: A vectorized conditional function.
- **`case_when`**: A general vectorized conditional function.
- **`bind_cols`**: Combine data frames by columns.
- **`bind_rows`**: Combine data frames by rows.
- **`filter`**: Filter rows based on conditions.
- **`slice`**: Select rows by position.
- **`arrange`**: Arrange rows by values of a column or multiple columns.
- **`group_by`**: Group data by one or more columns.
- **`ungroup`**: Remove grouping from data.
- **`group_keys`**: Retrieve the group keys from a grouped data frame.
- **`distinct`**: Return unique rows based on certain columns.

### From the `tibble` package:
- **`as_tibble`**: Convert data to a tibble.

## Privacy control levels
DataSHIELD implements [privacy control levels](https://wiki.datashield.org/en/opmanag/privacy-control-level), which allows data owners to control which functions can be
used by researchers. The table below shows which dsTidyverse functions are permitted in which
privacy mode.

| **Function**       | **Permissive** | **Banana** | **Avocado** | **Non-Permissive** |
|------------------|-------------|---------|---------|---------------|
| `arrangeDS` | ✔ | ✔ |  |  |
| `asTibbleDS` | ✔ | ✔ | ✔  | ✔ |
| `bindColsDS` | ✔ | ✔ |  |  |
| `bindRowsDS` | ✔ | ✔ |  |  |
| `caseWhenDS` | ✔ | ✔ |  |  |
| `distinctDS` | ✔ | ✔ | ✔  | ✔ |
| `filterDS` | ✔ | ✔ |  |  |
| `groupByDS` | ✔ | ✔ |  |  |
| `groupKeysDS` | ✔ | ✔ |  |  |
| `mutateDS` | ✔ | ✔  |  |  |
| `renameDS` | ✔ | ✔ | ✔  | ✔ |
| `selectDS` | ✔ | ✔ | ✔  | ✔ |
| `sliceDS` | ✔ | ✔ |  |  |
| `ungroupDS` | ✔ | ✔ |  |  |

## Contributing
If there are functions in this list you would like implemented, please either attempt to do so 
yourself and submit a pull request, or submit a feature request in the `issues` section. See 
the "CONTRIBUTING" file for more information.
