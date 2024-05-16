# .check_data_name_length throws an error if length of .data exceeds nfilter.string

    Code
      .check_data_name_length(.data, disc_settings, conns)
    Condition
      Error in `.check_data_name_length()`:
      ! Error: The length of string passed to `.data` must be less than nfilter.string.
      x The values of nfilter.string are:
      i sim1: 80
      i sim2: 80
      i sim3: 80
      x The length of `.data` is:
      i 101

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
      .check_variable_length(large_var, disc_settings, conns)
    Condition
      Error:
      ! Error: The maximum length of columns specified in `tidy_select` must be shorter than nfilter.string.
      x The values of nfilter.string are:
      i sim1: 80
      i sim2: 80
      i sim3: 80
      x These variables are longer than this:
      i sim1: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      i sim2: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      i sim3: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

# .tidy_disclosure_checks blocks argument with unpermitted variable length

    Code
      .tidy_disclosure_checks(arg_unpermitted_2, nfilter.string = 100)
    Condition
      Error in `.tidy_disclosure_checks()`:
      ! unused argument (nfilter.string = 100)

# .tidy_disclosure_checks blocks argument with unpermitted function names

    Code
      .tidy_disclosure_checks(arg_unpermitted_3, nfilter.string = 100)
    Condition
      Error in `.tidy_disclosure_checks()`:
      ! unused argument (nfilter.string = 100)

