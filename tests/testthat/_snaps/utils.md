# .check_data_name_length throws an error if length of .data exceeds nfilter.string

    Code
      .check_data_name_length(.data, nfilter.string)
    Condition
      Error in `.check_data_name_length()`:
      ! The length of string passed to `.data` must be less than 100 characters.
      x `.data` has a length of 101 characters.

# .check_function_names blocks unpermitted function names

    Code
      .check_function_names(arg_unpermitted)
    Condition
      Error:
      ! `tidy_select` must only contain Tidyverse select functions
      x You have included the following unpermitted functions: filter, slice, and mutate
      Search ?select for more information

# .check_variable_length blocks variables with value greater than than nfilter.string

    Code
      .check_variable_length(large_var, 100)
    Condition
      Error in `.check_variable_length()`:
      ! The maximum length of columns specified in `tidy_select` is 100 characters.
      x Detected 1 variable longer than this: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

# .tidy_disclosure_checks blocks argument with unpermitted variable length

    Code
      .tidy_disclosure_checks(arg_unpermitted_2, nfilter.string = 100)
    Condition
      Error in `.check_variable_length()`:
      ! The maximum length of columns specified in `tidy_select` is 100 characters.
      x Detected 1 variable longer than this: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaasd

# .tidy_disclosure_checks blocks argument with unpermitted function names

    Code
      .tidy_disclosure_checks(arg_unpermitted_3, nfilter.string = 100)
    Condition
      Error:
      ! `tidy_select` must only contain Tidyverse select functions
      x You have included the following unpermitted functions: filter, slice, and mutate
      Search ?select for more information

