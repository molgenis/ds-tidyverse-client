## Version 1.1.0
* Added `%` encoding support for date format strings, enabling date formats (e.g. `"%Y-%m-%d"`) to be passed through the parser in functions like `ds.mutate`

## Version 1.0.2
* Introduced stricter privacy checks to block potential inference attacks

## Version 1.0.0

* Initial release of the package with the following functions included:
`select`, `rename`, `mutate`, `if_else`, `case_when`, `bind_cols`, `bind_rows`, `filter`, `slice`, 
`arrange`, `group_by`, `ungroup`, `group_keys`
