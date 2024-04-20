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