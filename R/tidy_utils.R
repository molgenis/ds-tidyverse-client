.getEncodeKey <- function() {
  encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|"),
    output = c("$LIST$", "$LB", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$")
  )
  return(encode_list)
}

.encode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- setNames(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}

.remove_list <- function(string_decoded) {
  string_decoded %>%
    str_replace_all(pattern = fixed("list("), replacement = "") %>%
    str_sub(end = -2)
}

.format_args_as_string <- function(expr) {
  args_as_string <- quo_text(expr) ## Convert them to a string
  neat_args_as_string <- .remove_list(args_as_string)
  return(neat_args_as_string)
}



.check_function_names <- function(args_as_string){

  permitted_tidy_select <- c("everything", "last_col", "group_cols", "starts_with", "ends_with", "contains",
                             "matches", "num_range", "all_of", "any_of", "where")

  function_names <- str_extract_all(args_as_string, "\\w+(?=\\()", simplify = T)
  any_banned_functions <- function_names[!function_names %in% permitted_tidy_select]
  if(length(any_banned_functions) > 0) {
    stop("The provided tidy select syntax contains the following functions which are not permitted",
         any_banned_functions, "Only functions permitted are Tidyverse selection functions. Search
           ?select for more information")
  }
}

.check_variable_length <- function(args_as_string, nfilter.string){

  variable_names <- str_extract_all(text, "\\b\\w+\\b(?!\\()", simplify = T)
  variable_lengths <- variable_names %>% map_int(str_length)
  over_filter_thresh <- variable_lengths %>% map_lgl(~. > nfilter.string)
  too_long <- variable_names[over_filter_thresh]

  if(length(too_long) > 0 ){
    stop("The provided tidy select syntax contains the following variable names with a length >
         nfilter.string.", too_long, "Please correct and try again.")
  }
}

.tidy_disclosure_checks <- function(args_as_string, nfilter.string){
  .check_function_names(args_as_string)
  .check_variable_length(args_as_string, nfilter.string)
}

