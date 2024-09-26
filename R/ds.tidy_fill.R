#' @title Order the rows of a data frame by the values of selected columns
#' @description DataSHIELD implentation of  \code{dplyr::arrange}.
#' @param df.name Character specifying a serverside data frame or tibble.
#' @param expr Variables, or functions of variables. Use desc() to sort a variable in descending
#' order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames
#' only.
#' @param newobj Character specifying name for new server-side data frame.
#' @param datasources DataSHIELD connections object.
#' @return An object with the name specified by the \code{newobj} argument is written serverside.
#' @importFrom DSI datashield.assign datashield.aggregate
#' @examples
#'\dontrun{
#'ds.arrange(
#'  "mtcars",
#'  expr = list(drat),
#'  newobj = "sorted_df"
#'  )
#' }
#' @export
ds.tidy_fill <- function(df.name = NULL, newobj = NULL, datasources = NULL) {
  datasources <- .set_datasources(datasources)
  # .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  col_names <- datashield.aggregate(datasources, call("colnamesDS", df.name))
  .stop_if_cols_identical(col_names)

  var_classes <- .get_var_classes(df.name, datasources)
  different_classes <- .identify_class_conflicts(var_classes)

  if(length(different_classes) > 0) {

    class_decisions <- prompt_user_class_decision_all_vars(
      names(different_classes),
      var_classes$server,
      dplyr::select(var_classes, all_of(names(different_classes)))
    )

    .fix_classes(df.name, different_classes, class_decisions, newobj, datasources)
  }

  unique_cols <- .get_unique_cols(col_names)
  .add_missing_cols_to_df(df.name, unique_cols, newobj, datasources)

  new_names <- datashield.aggregate(datasources, call("colnamesDS", newobj))
  added_cols <- .get_added_cols(col_names, new_names)

  new_classes <- .get_var_classes(df.name, datasources)

  factor_vars <- .identify_factor_vars(new_classes)
  factor_levels <- .get_factor_levels(factor_vars, newobj, datasources)
  level_conflicts <- .identify_level_conflicts(factor_levels)

  if(length(level_conflicts) > 0) {
    levels_decision <- ask_question_wait_response_levels(level_conflicts)
  }

  if(levels_decision == "1") {
    unique_levels <- .get_unique_levels(factor_levels, level_conflicts)
    .set_factor_levels(newobj, unique_levels, datasources)
  }

  cli_alert_success("The following variables have been added to {newobj}:")
  added_cols_neat <- added_cols %>% map(~ifelse(length(.) == 0, "", .))
  var_message <- paste0(names(added_cols), " --> ", added_cols_neat)
  for(i in 1: length(var_message)) {
    cli_alert_info("{var_message[[i]]}")
  }
  cli_text("")

  if(length(different_classes) > 0) {
    .print_class_recode_message(class_decisions, different_classes, newobj)
    cli_text("")
  }

  if(length(level_conflicts) > 0 & levels_decision == "1") {
    .print_levels_recode_message(unique_levels, newobj)
  }
}

.get_added_cols <- function(old_names, new_names) {
  list(old_names, new_names) %>%
    pmap(function(.x, .y) {
      .y[!.y %in% .x]
    })
}

prompt_user_class_decision_all_vars <- function(vars, all_servers, all_classes) {
  decisions <- c()
  for(i in 1: length(vars)) {
    decisions[i] <- prompt_user_class_decision(vars[i], all_servers, all_classes[[i]])
  }
  return(decisions)
}

prompt_user_class_decision <- function(var, all_servers, all_classes) {
  cli_alert_warning("`ds.dataFrameFill` requires that all columns have the same class.")
  cli_alert_danger("Column {.strong {var}} has following classes:")
  print_all_classes(all_servers, all_classes)
  cli_text("")
  return(ask_question_wait_response_class(var))
}

.print_class_recode_message <- function(class_decisions, different_classes, newobj) {
  choice_neat <- change_choice_to_string(class_decisions)
  class_message <- paste0(names(different_classes), " --> ", choice_neat)
  cli_alert_success("The following classes have been set in {newobj}: ")
  for(i in 1: length(class_message)) {
    cli_alert_info("{class_message[[i]]}")
  }
}

.print_levels_recode_message <- function(unique_levels, newobj) {
  levels_message <- .make_levels_recode_message(unique_levels)
  cli_alert_success("The following levels have been set in {newobj}: ")
  for(i in 1: length(levels_message)) {
    cli_alert_info("{levels_message[[i]]}")
  }
}

.make_levels_recode_message <- function(unique_levels) {
  return(
    list(names(unique_levels), unique_levels) %>%
      pmap(function(.x, .y) {
        paste0(.x, " --> ", paste0(.y, collapse = ", "))
      })
  )
}


  .fix_classes <- function(df.name, different_classes, class_decisions, newobj, datasources) {
    cally <- call("fixClassDS", df.name, names(different_classes), class_decisions)
    datashield.assign(datasources, newobj, cally)
  }


 .set_factor_levels <- function(newobj, unique_levels, datasources) {
   cally <- call("setAllLevelsDS", newobj, names(unique_levels), unique_levels)
   datashield.assign(datasources, newobj, cally)
 }

.get_factor_levels <- function(factor_vars, newobj, datasources) {
  cally <- call("getAllLevelsDS", newobj, names(factor_vars))
  return(datashield.aggregate(datasources, cally))
}


.get_unique_levels <- function(factor_levels, level_conflicts) {
  unique_levels <- factor_levels %>%
    map(~.[level_conflicts]) %>%
    pmap(function(...) {

      as.character(c(...))

    }) %>%
    map(~unique(.))
  return(unique_levels)
}

change_choice_to_string <- function(class_decision) {
  case_when(
    class_decision == "1" ~ "factor",
    class_decision == "2" ~ "integer",
    class_decision == "3" ~ "numeric",
    class_decision == "4" ~ "character",
    class_decision == "5" ~ "logical")
}

ask_question_wait_response_levels <- function(level_conflicts) {
  .make_levels_message(level_conflicts)
  answer <- give_prompt()
  return(check_response_levels(answer, level_conflicts))
}

check_response_levels <- function(answer, level_conflicts) {
  if(!answer %in% as.character(1:2)) {
    cli_alert_warning("Invalid input. Please try again.")
    cli_alert_info("")
    .make_levels_message(level_conflicts)
  } else {
    return(answer)
  }
}

.make_levels_message <- function(level_conflicts) {
  cli_alert_warning("Warning: factor variables {level_conflicts} do not have the same levels in all studies")
  cli_alert_info("Would you like to:")
  cli_ol(c("Create the missing levels where they are not present", "Do nothing"))
}

.identify_level_conflicts <- function(factor_levels) {

    levels <- factor_levels %>%
      pmap_lgl(function(...) {
        args <- list(...)
        !all(map_lgl(args[-1], ~ identical(.x, args[[1]])))
      })

    return(names(levels[levels == TRUE]))
}

.identify_factor_vars <- function(var_classes) {
  return(
    var_classes %>%
      filter(row_number() == 1) %>%
      select(where(~ . == "factor"))
  )
}

print_all_classes <- function(all_servers, all_classes) {
  combined <- paste(all_servers, all_classes, sep = ": ")
  cli_ul()
  for(i in 1: length(combined)) {
    cli_li("{combined[i]}")
  }
  cli_end()
}

ask_question_wait_response_class <- function(question) {
  ask_question(question)
  answer <- give_prompt()
  return(check_response_class(answer))
}

ask_question <- function(var){
  cli_alert_info("Would you like to:")
  class_options <- c("a factor", "an integer", "numeric", "a character", "a logical vector")
  class_message <- paste0("Convert `{var}` to ",  class_options, " in all studies")
  cli_ol(
    c(class_message, "Cancel `ds.dataFrameFill` operation")
  )
}

give_prompt <- function() {
  readline()
}

check_response_class <- function(answer, var) {
  if(answer == "6"){
    cli_abort("Aborted `ds.dataFrameFill`", .call = NULL)
  } else if(!answer %in% as.character(1:5)) {
    cli_alert_warning("Invalid input. Please try again.")
    cli_alert_info("")
    question(var)
  } else {
    return(answer)
  }
}

.stop_if_cols_identical <- function(col_names) {
  are_identical <- Reduce(identical, col_names)
  if(are_identical) {
    cli_abort("Columns are identical in all data frames: nothing to fill")
  }
}

.get_unique_cols <- function(col_names){
  return(
    unique(
      unlist(col_names))
  )
}

.get_var_classes <- function(df.name, datasources) {
  cally <- call("classAllColsDS", df.name)
  classes <- datashield.aggregate(datasources, cally) %>%
    bind_rows(.id = "server")
  return(classes)
}

.add_missing_cols_to_df <- function(df.name, unique_cols, newobj, datasources) {
  cally <- call("makeColsSameDS", df.name, unique_cols)
  datashield.assign(datasources, newobj, cally)

}

.identify_class_conflicts <- function(classes) {

  different_class <- classes |>
    dplyr::select(-server) |>
    map(~unique(na.omit(.)))

  out <- different_class[which(different_class %>% map(length) > 1)]
  return(out)
}
