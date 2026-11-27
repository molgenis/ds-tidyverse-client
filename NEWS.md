## Version 1.1.2
* Fixed a bug where expressions not wrapped in `list()` were silently altered before being sent to the server, for example `mpg > 20` became `mpg > 2`. These now raise an error. Please check any analyses run with earlier versions without `list()`
* Fixed a bug where every `list(` within an expression was removed, not just the outer one
* `newobj` must now be a single non-empty character string, and `ds.select` now checks `newobj` and `df.name`

## Version 1.1.0
* Added `%` encoding support for date format strings, enabling date formats (e.g. `"%Y-%m-%d"`) to be passed through the parser in functions like `ds.mutate`

## Version 1.0.2
* Introduced stricter privacy checks to block potential inference attacks

## Version 1.0.0

* Initial release of the package with the following functions included:
`select`, `rename`, `mutate`, `if_else`, `case_when`, `bind_cols`, `bind_rows`, `filter`, `slice`, 
`arrange`, `group_by`, `ungroup`, `group_keys`
