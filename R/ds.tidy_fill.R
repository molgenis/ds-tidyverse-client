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
ds.tidy_fill <- function(df.name = NULL, fix_class = FALSE, newobj = NULL, datasources = NULL) {

  datasources <- .set_datasources(datasources)
  # .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)
  browser()

  col_names <- datashield.aggregate(datasources, call("colnamesDS", df.name))
  .stop_if_cols_identical(col_names)

  var_classes <- .get_var_classes(df.name, datasources)
  different_classes <- .identify_class_conflicts(var_classes)
  .stop_if_class_problems(different_classes, fix_class)

  unique_cols <- .get_unique_cols(col_names)
  .add_missing_cols_to_df(df.name, unique_cols, newobj, datasources)




  target_vars <- names(different_classes)
  target_classes <- different_classes %>% map_chr(~.[[1]])

  na_vars <- var_classes[var_classes %>% map_lgl(~any(is.na(.)))]
  na_class <- as.character(na_vars[1, ])

  cally <- call("fixClassDS", newobj, c(target_vars, names(na_vars)), c(unname(target_classes), na_class))
  datashield.assign(datasources, newobj, cally)


  factor_vars <- classes_neat %>%
    filter(row_number() == 1) %>%               # Select the first row
    select(where(~ . == "factor"))

  cally <- call("getAllLevelsDS", newobj, names(factor_vars))
  factor_levels <- datashield.aggregate(datasources, cally)

  level_match <- factor_levels %>%
    pmap_lgl(function(...) {
      args <- list(...)
      all(map_lgl(args[-1], ~ identical(.x, args[[1]])))
    })

  unique_levels <- factor_levels %>%
    map(~.[!level_match]) %>%
    pmap(function(...) {

      as.character(c(...))

    }) %>%
    map(~unique(.))

  cally <- call("setAllLevelsDS", newobj, names(unique_levels), unique_levels)
  datashield.assign(datasources, newobj, cally)

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

  .stop_if_class_problems <- function(different_classes, fix_class) {

    if(length(different_classes) > 0 & fix_class == F) {
      cli_abort(
        c(
          "x" = "Cannot fill columns where `fix_class` is FALSE and classes are not identical for all servers",
          "i" = "fix_class is FALSE",
          "i" = "Variables {names(different_classes)} do not share the same class"
        )
      )
    }
  }



