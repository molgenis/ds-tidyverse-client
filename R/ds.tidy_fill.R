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
ds.tidy_fill <- function(df.name = NULL, fix_class = NULL, newobj = NULL, datasources = NULL) {
  browser()
  datasources <- .set_datasources(datasources)
  # .perform_tidyverse_checks(df.name, newobj, tidy_select, datasources)

  unique_cols <- .get_unique_cols(df.name, datasources)
  .make_cols_same(df.name, unique_cols, newobj, datasources)

  var_classes <- .get_var_classes(df.name, datasources)
  classes_neat <- .format_var_classes(var_classes)
  different_class <- .identify_class_conflict(classes_neat)

  if(length(different_class) > 0 & fix_class == F) {
    cli_abort("The following variables have conflicting classes for different servers and
              `fix_class` is set to false: {names(different_class)}")
  }

  target_vars <- names(different_class)
  target_classes <- different_class %>% map_chr(~.[[1]])

  na_vars <- classes_neat[classes_neat %>% map_lgl(~any(is.na(.)))]
  na_class <- as.character(na_vars[1, ])

  cally <- call("fixClassDS", newobj, c(target_vars, names(na_vars)), c(unname(target_classes), na_class))

  datashield.assign(datasources, newobj, cally)


  factor_vars <- classes_neat %>%
    filter(row_number() == 1) %>%               # Select the first row
    select(where(~ . == "factor"))

  cally <- call("getAllLevelsDS", newobj, names(factor_vars))
  datashield.assign(datasources, newobj, cally)




  levelsAllColsDS


  browser()
}






  .identify_class_conflict <- function(classes_neat) {

    different_class <- classes_neat |>
      dplyr::select(-server) |>
      map(~unique(na.omit(.)))

    out <- different_class[which(different_class %>% map(length) > 1)]
    return(out)
  }


  .format_var_classes <- function(classes_neat) {
    out <- classes_neat |>
      map(as_tibble) |>
      bind_rows(.id = "server")
    return(out)
  }


.get_var_classes <- function(df.name, datasources) {
  cally <- call("classAllColsDS", df.name)
  classes <- datashield.aggregate(datasources, cally)
  return(classes)
}

.make_cols_same <- function(df.name, unique_cols, newobj, datasources) {
cally <- call("makeColsSameDS", df.name, unique_cols)
datashield.assign(datasources, newobj, cally)

}

.get_unique_cols <- function(df.name, datasources){
  cols <- datashield.aggregate(datasources, call("colnamesDS", df.name))
  return(
    unique(
      unlist(cols))
  )
}
