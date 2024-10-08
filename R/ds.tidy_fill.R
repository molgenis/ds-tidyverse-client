#' Fill DataFrame with Missing Columns and Adjust Classes
#'
#' This function fills a given DataFrame by adding missing columns, ensuring consistent column classes, and adjusting factor levels where necessary.
#' It performs checks to detect class and factor level conflicts and prompts the user for decisions to resolve these conflicts.
#'
#' @param df.name Name of the input DataFrame to fill.
#' @param newobj Name of the new DataFrame object created after filling.
#' @param datasources Data sources from which to aggregate data. Default is \code{NULL}.
#'
#' @import dplyr
#' @import purrr
#' @import datashield
#' @return The filled DataFrame with added columns and adjusted classes or factor levels.
#' @examples
#' ds.tidy_fill(df.name = "df1", newobj = "filled_df", datasources = conns)
ds.tidy_fill <- function(df.name = NULL, newobj = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)

  assert_that(is.character("df.name"))
  assert_that(is.character("newobj"))

  col_names <- datashield.aggregate(datasources, call("colnamesDS", df.name))
  .stop_if_cols_identical(col_names)

  var_classes <- .get_var_classes(df.name, datasources)
  class_conflicts <- .identify_class_conflicts(var_classes)

  if (length(class_conflicts) > 0) {
    class_decisions <- prompt_user_class_decision_all_vars(
      names(class_conflicts),
      var_classes$server,
      dplyr::select(var_classes, all_of(names(class_conflicts)))
    )
    .fix_classes(df.name, class_conflicts, class_decisions, newobj, datasources)
  }

  unique_cols <- .get_unique_cols(col_names)
  .add_missing_cols_to_df(df.name, unique_cols, newobj, datasources)
  added_cols <- .summarise_new_cols(newobj, datasources, col_names)

  new_classes <- .get_var_classes(df.name, datasources)
  factor_vars <- .identify_factor_vars(new_classes)
  factor_levels <- .get_factor_levels(factor_vars, newobj, datasources)
  level_conflicts <- .identify_level_conflicts(factor_levels)

  if (length(level_conflicts) > 0) {
    levels_decision <- ask_question_wait_response_levels(level_conflicts)
  }

  if (levels_decision == "1") {
    unique_levels <- .get_unique_levels(factor_levels, level_conflicts)
    .set_factor_levels(newobj, unique_levels, datasources)
  }

  .print_out_messages(added_cols, class_decisions, class_conflicts, unique_levels,
                      level_conflicts, levels_decision, newobj)
}

#' Stop If Columns Are Identical
#'
#' Checks if the columns in the data frames are identical and throws an error if they are.
#'
#' @param col_names A list of column names from different data sources.
#' @return None. Throws an error if columns are identical.
#' @importFrom cli cli_abort
.stop_if_cols_identical <- function(col_names) {
  are_identical <- all(sapply(col_names, identical, col_names[[1]]))
  if (are_identical) {
    cli_abort("Columns are identical in all data frames: nothing to fill")
  }
}

#' Get Variable Classes from DataFrame
#'
#' Retrieves the class of each variable in the specified DataFrame from different data sources.
#'
#' @param df.name Name of the input DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return A DataFrame containing the variable classes from each data source.
#' @import dplyr
.get_var_classes <- function(df.name, datasources) {
  cally <- call("classAllColsDS", df.name)
  classes <- datashield.aggregate(datasources, cally) %>%
    bind_rows(.id = "server")
  return(classes)
}

#' Identify Class Conflicts
#'
#' Identifies conflicts in variable classes across different data sources.
#'
#' @param classes A DataFrame containing variable classes across data sources.
#' @return A list of variables that have class conflicts.
#' @import dplyr
#' @importFrom purrr map
.identify_class_conflicts <- function(classes) {
  different_class <- classes |>
    dplyr::select(-server) |>
    map(~ unique(na.omit(.)))

  out <- different_class[which(different_class %>% map(length) > 1)]
  return(out)
}

#' Prompt User for Class Decision for All Variables
#'
#' Prompts the user to resolve class conflicts for all variables.
#'
#' @param vars A vector of variable names with class conflicts.
#' @param all_servers The names of all servers.
#' @param all_classes The classes of the variables across servers.
#' @return A vector of decisions for each variable's class.
prompt_user_class_decision_all_vars <- function(vars, all_servers, all_classes) {
  decisions <- c()
  for (i in 1:length(vars)) {
    decisions[i] <- prompt_user_class_decision(vars[i], all_servers, all_classes[[i]])
  }
  return(decisions)
}

#' Prompt User for Class Decision for a Single Variable
#'
#' Prompts the user to resolve a class conflict for a single variable.
#'
#' @param var The variable name with a class conflict.
#' @param all_servers The names of all servers.
#' @param all_classes The classes of the variable across servers.
#' @return A decision for the variable's class.
prompt_user_class_decision <- function(var, all_servers, all_classes) {
  cli_alert_warning("`ds.dataFrameFill` requires that all columns have the same class.")
  cli_alert_danger("Column {.strong {var}} has following classes:")
  print_all_classes(all_servers, all_classes)
  cli_text("")
  return(ask_question_wait_response_class(var))
}

#' Check User Response for Class Decision
#'
#' Checks the user's input to ensure it is valid for class decisions.
#'
#' @param answer The user's input.
#' @param var The variable name.
#' @return The user's decision or a recursive prompt for input.
check_response_class <- function(answer, var) {
  if (answer == "6") {
    cli_abort("Aborted `ds.dataFrameFill`", .call = NULL)
  } else if (!answer %in% as.character(1:5)) {
    cli_alert_warning("Invalid input. Please try again.")
    cli_alert_info("")
    question(var)
  } else {
    return(answer)
  }
}

#' Ask Question and Wait for Class Response
#'
#' Prompts the user with a question and waits for a response related to class decisions.
#'
#' @param question The question to ask the user.
#' @return The user's decision.
ask_question_wait_response_class <- function(question) {
  ask_question(question)
  answer <- give_prompt()
  return(check_response_class(answer))
}

#' Fix Variable Classes
#'
#' Applies the user's class decisions to fix the classes of variables across different data sources.
#'
#' @param df.name The name of the DataFrame.
#' @param different_classes A list of variables with class conflicts.
#' @param class_decisions The decisions made by the user.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with consistent variable classes.
.fix_classes <- function(df.name, different_classes, class_decisions, newobj, datasources) {
  cally <- call("fixClassDS", df.name, names(different_classes), class_decisions)
  datashield.assign(datasources, newobj, cally)
}

#' Get Unique Columns from Data Sources
#'
#' Retrieves all unique columns from the data sources.
#'
#' @param col_names A list of column names.
#' @return A vector of unique column names.
.get_unique_cols <- function(col_names) {
  return(
    unique(
      unlist(col_names)
    )
  )
}

#' Add Missing Columns to DataFrame
#'
#' Adds any missing columns to the DataFrame to ensure all columns are present across data sources.
#'
#' @param df.name The name of the DataFrame.
#' @param unique_cols A vector of unique column names.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with added columns.
.add_missing_cols_to_df <- function(df.name, unique_cols, newobj, datasources) {
  cally <- call("makeColsSameDS", df.name, unique_cols)
  datashield.assign(datasources, newobj, cally)
}

#' Summarize Newly Added Columns
#'
#' Summarizes the columns that were added to the DataFrame.
#'
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @param col_names A list of column names.
#' @return A list of added columns.
.summarise_new_cols <- function(newobj, datasources, col_names) {
  new_names <- datashield.aggregate(datasources, call("colnamesDS", newobj))
  return(.get_added_cols(col_names, new_names))
}

#' Get Added Columns
#'
#' Compares the old and new column names and identifies newly added columns.
#'
#' @param old_names A list of old column names.
#' @param new_names A list of new column names.
#' @return A list of added column names.
.get_added_cols <- function(old_names, new_names) {
  list(old_names, new_names) %>%
    pmap(function(.x, .y) {
      .y[!.y %in% .x]
    })
}

#' Identify Factor Variables
#'
#' Identifies which variables are factors in the DataFrame.
#'
#' @param var_classes A DataFrame containing variable classes.
#' @return A vector of factor variables.
.identify_factor_vars <- function(var_classes) {
  return(
    var_classes %>%
      filter(row_number() == 1) %>%
      select(where(~ . == "factor"))
  )
}

#' Get Factor Levels from Data Sources
#'
#' Retrieves the levels of factor variables from different data sources.
#'
#' @param factor_vars A vector of factor variables.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return A list of factor levels.
.get_factor_levels <- function(factor_vars, newobj, datasources) {
  cally <- call("getAllLevelsDS", newobj, names(factor_vars))
  return(datashield.aggregate(datasources, cally))
}

#' Identify Factor Level Conflicts
#'
#' Identifies conflicts in factor levels across different data sources.
#'
#' @param factor_levels A list of factor levels.
#' @return A list of variables with level conflicts.
.identify_level_conflicts <- function(factor_levels) {
  levels <- factor_levels %>%
    pmap_lgl(function(...) {
      args <- list(...)
      !all(map_lgl(args[-1], ~ identical(.x, args[[1]])))
    })

  return(names(levels[levels == TRUE]))
}

#' Ask Question and Wait for Response on Factor Levels
#'
#' Prompts the user with options for resolving factor level conflicts and waits for a response.
#'
#' @param level_conflicts A list of variables with factor level conflicts.
#' @return The user's decision.
ask_question_wait_response_levels <- function(level_conflicts) {
  .make_levels_message(level_conflicts)
  answer <- give_prompt()
  return(check_response_levels(answer, level_conflicts))
}

#' Make Factor Level Conflict Message
#'
#' Creates a message to alert the user about factor level conflicts and prompt for action.
#'
#' @param level_conflicts A list of variables with factor level conflicts.
#' @return None. Prints the message to the console.
.make_levels_message <- function(level_conflicts) {
  cli_alert_warning("Warning: factor variables {level_conflicts} do not have the same levels in all studies")
  cli_alert_info("Would you like to:")
  cli_ol(c("Create the missing levels where they are not present", "Do nothing"))
}

#' Check User Response for Factor Levels
#'
#' Checks the user's input to ensure it is valid for resolving factor level conflicts.
#'
#' @param answer The user's input.
#' @param level_conflicts A list of variables with factor level conflicts.
#' @return The user's decision.
check_response_levels <- function(answer, level_conflicts) {
  if (!answer %in% as.character(1:2)) {
    cli_alert_warning("Invalid input. Please try again.")
    cli_alert_info("")
    .make_levels_message(level_conflicts)
  } else {
    return(answer)
  }
}

#' Get Unique Factor Levels
#'
#' Retrieves the unique factor levels for variables with conflicts.
#'
#' @param factor_levels A list of factor levels.
#' @param level_conflicts A list of variables with level conflicts.
#' @return A list of unique factor levels.
.get_unique_levels <- function(factor_levels, level_conflicts) {
  unique_levels <- factor_levels %>%
    map(~ .[level_conflicts]) %>%
    pmap(function(...) {
      as.character(c(...))
    }) %>%
    map(~ unique(.))
  return(unique_levels)
}

#' Set Factor Levels in DataFrame
#'
#' Applies the unique factor levels to the DataFrame.
#'
#' @param newobj The name of the new DataFrame.
#' @param unique_levels A list of unique factor levels.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with the new factor levels.
.set_factor_levels <- function(newobj, unique_levels, datasources) {
  cally <- call("setAllLevelsDS", newobj, names(unique_levels), unique_levels)
  datashield.assign(datasources, newobj, cally)
}

#' Print Out Summary Messages
#'
#' Prints summary messages regarding the filled DataFrame, including added columns, class decisions, and factor level adjustments.
#'
#' @param added_cols A list of added columns.
#' @param class_decisions A vector of class decisions.
#' @param different_classes A list of variables with class conflicts.
#' @param unique_levels A list of unique factor levels.
#' @param level_conflicts A list of variables with level conflicts.
#' @param levels_decision The decision made regarding factor levels.
#' @param newobj The name of the new DataFrame.
#' @return None. Prints messages to the console.
.print_out_messages <- function(added_cols, class_decisions, different_classes, unique_levels,
                                level_conflicts, levels_decision, newobj) {
  .print_var_recode_message(added_cols, newobj)

  if (length(different_classes) > 0) {
    .print_class_recode_message(class_decisions, different_classes, newobj)
    cli_text("")
  }

  if (length(level_conflicts) > 0 & levels_decision == "1") {
    .print_levels_recode_message(unique_levels, newobj)
  }
}

#' Print Variable Recode Message
#'
#' Prints a message summarizing the columns that were added to the DataFrame.
#'
#' @param added_cols A list of added columns.
#' @param newobj The name of the new DataFrame.
#' @return None. Prints the message to the console.
.print_var_recode_message <- function(added_cols, newobj) {
  cli_alert_success("The following variables have been added to {newobj}:")
  added_cols_neat <- added_cols %>% map(~ ifelse(length(.) == 0, "", .))
  var_message <- paste0(names(added_cols), " --> ", added_cols_neat)
  for (i in 1:length(var_message)) {
    cli_alert_info("{var_message[[i]]}")
  }
  cli_text("")
}

#' Print Class Recode Message
#'
#' Prints a message summarizing the class decisions that were made for variables with conflicts.
#'
#' @param class_decisions A vector of class decisions.
#' @param different_classes A list of variables with class conflicts.
#' @param newobj The name of the new DataFrame.
#' @return None. Prints the message to the console.
.print_class_recode_message <- function(class_decisions, different_classes, newobj) {
  choice_neat <- change_choice_to_string(class_decisions)
  class_message <- paste0(names(different_classes), " --> ", choice_neat)
  cli_alert_success("The following classes have been set in {newobj}: ")
  for (i in 1:length(class_message)) {
    cli_alert_info("{class_message[[i]]}")
  }
}

#' Print Factor Levels Recode Message
#'
#' Prints a message summarizing the factor level decisions that were made for variables with conflicts.
#'
#' @param unique_levels A list of unique factor levels.
#' @param newobj The name of the new DataFrame.
#' @return None. Prints the message to the console.
.print_levels_recode_message <- function(unique_levels, newobj) {
  levels_message <- .make_levels_recode_message(unique_levels)
  cli_alert_success("The following levels have been set in {newobj}: ")
  for (i in 1:length(levels_message)) {
    cli_alert_info("{levels_message[[i]]}")
  }
}

#' Make Levels Recode Message
#'
#' Creates a message to alert the user about factor level recoding.
#'
#' @param unique_levels A list of unique factor levels.
#' @return A formatted string summarizing the level recoding.
.make_levels_recode_message <- function(unique_levels) {
  return(
    list(names(unique_levels), unique_levels) %>%
      pmap(function(.x, .y) {
        paste0(.x, " --> ", paste0(.y, collapse = ", "))
      })
  )
}





# change_choice_to_string <- function(class_decision) {
#   case_when(
#     class_decision == "1" ~ "factor",
#     class_decision == "2" ~ "integer",
#     class_decision == "3" ~ "numeric",
#     class_decision == "4" ~ "character",
#     class_decision == "5" ~ "logical"
#   )
# }

# print_all_classes <- function(all_servers, all_classes) {
#   combined <- paste(all_servers, all_classes, sep = ": ")
#   cli_ul()
#   for (i in 1:length(combined)) {
#     cli_li("{combined[i]}")
#   }
#   cli_end()
# }

# ask_question <- function(var) {
#   cli_alert_info("Would you like to:")
#   class_options <- c("a factor", "an integer", "numeric", "a character", "a logical vector")
#   class_message <- paste0("Convert `{var}` to ", class_options, " in all studies")
#   cli_ol(
#     c(class_message, "Cancel `ds.dataFrameFill` operation")
#   )
# }

