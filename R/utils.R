#' Retrieve datasources if not specified
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @return A list of data sources.
#' @noRd
.get_datasources <- function(datasources){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
    return(datasources)
  }
}

#' Verify that the provided data sources are of class 'DSConnection'.
#'
#' @param datasources A list of data sources.
#' @importFrom cli cli_abort
#' @noRd
.verify_datasources <- function(datasources){

  is_connection_class <- datasources %>% map_lgl(~unlist(.x) %>% methods::is("DSConnection"))
  if(!all(is_connection_class)){
    cli::abort("The 'datasources' were expected to be a list of DSConnection-class objects")
  }
}

#' Set and verify data sources.
#'
#' @param datasources An optional list of data sources. If not provided, the function will attempt
#' to find available data sources.
#' @return A list of verified data sources.
#' @noRd
.set_datasources <- function(datasources){
  datasources <- .get_datasources(datasources)
  .verify_datasources(datasources)
  return(datasources)
}


#' Set a new object or defaults to '.data' if no object is provided.
#'
#' @param newobj An optional new object name. If not provided, the function defaults to '.data'.
#' @return The provided new object name or '.data' if no object is provided.
#' @noRd
.set_new_obj <- function(newobj){
  if(is.null(newobj)){
    newobj <- .data
  }
  return(newobj)
}

#' Check that the length of the character string provided to `.data` does not exceed the value of
#' nfilter.string
#'
#' @param .data The data object to be analyzed.
#' @param nfilter.string The maximum length of variable names allowed.
#' @noRd
.check_data_name_length <- function(.data, nfilter.string){
  data_length <- str_length(.data)
  if(data_length > nfilter.string){
    cli_abort(
      c("The length of string passed to `.data` must be less than {nfilter.string} characters.",
        "x" = "`.data` has a length of {data_length} characters.")
    )
  }
}
