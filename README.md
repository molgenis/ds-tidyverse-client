# dsTidyverseClient
This package implements selected functions from the Tidyverse family of R packages (https://www.tidyverse.org/).
It is designed to make data manipulation easier and more intuitive within DataSHIELD, so that researchers
can spend more time doing research and less time data wrangling.

## To get started
- Install the serverside package `dsTidyverse` (https://github.com/molgenis/ds-tidyverse) on your 
armadillo or opal server. If you are working as part of a consortium with managed profiles 
(e.g. ATHLETE), contact the project coordinator to discuss adding this to an installed profile.
- Install dsTidyverseClient on your local machine. Again, if you are working in a consortium whereby analysis is conducted via a Central Analysis Server, contact the project coordinator to discuss adding this package.

## Currently implemented:
The currently implemented functions are `dplyr::select`, `dplyr::rename` &`dplyr::mutate`

## Planned implementation:
The functions planned for implementation are `dplyr::if_else`, `dplyr::case_when`, `dplyr::filter`, 
`dplyr::bind_cols`, `dplyr::bind_rows`, `dplyr::inner_join`, `dplyr::left_join`, `dplyr::right_join`,
`dplyr::full_join`, `tidyr::pivot_longer`, `tidyr::pivot_wider`, `dplyr::arrange`, `dplyr::distinct`, 
`dplyr::group_by`, `dplyr::ungroup`, `dplyr::slice`, `dplyr::group_split`, `dplyr::group_map` & 
`dplyr::summarise`.

If there are functions not in this list that you would like to see implemented, please either attempt 
to do so yourself and submit a pull request, or submit a feature request in the `issues` section of
the github repository.
