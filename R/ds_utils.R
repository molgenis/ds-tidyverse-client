.set_datasources <- function(datasources){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
    return(datasources)
  }
}

.verify_datasources <- function(datasources){

  is_connection_class <- datasources %>% map_lgl(~unlist(.x) %>% methods::is("DSConnection"))
  if(!all(is_connection_class)){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
}

newobj = "test"

.get_obj_info <- function(datasources, newobj){
  cally <- call("testObjExistsDS", newobj)
  DSI::datashield.aggregate(datasources, cally)
}

# Function to check if object exists in all sources
check_obj_existence <- function(object_info) {
  object_info %>%
    map_lgl(~.$test.obj.exists) %>%
    all()
}

# Function to check if object has valid content/class
check_obj_validity <- function(object_info) {
  object_info %>%
    map_lgl(~!is.null(.$test.obj.class) && !("ABSENT" %in% .$test.obj.class)) %>%
    all()
}

# Function to generate return messages
generate_return_messages <- function(obj_name, obj_exists, obj_valid) {
  if (obj_exists && obj_valid) {
    return_message <- paste0("A data object <", obj_name, "> has been created in all specified data sources")
  } else {
    return_message <- list(
      paste0("Error: A valid data object <", obj_name, "> does NOT exist in ALL specified data sources"),
      paste0("It is either ABSENT and/or has no valid content/class, see return.info above"),
      "Please use ds.ls() to identify where missing"
    )
  }
  return(return_message)
}

# Function to call `messageDS` function and aggregate results
call_messageDS <- function(datasources, test_obj_name) {
  map(datasources, ~DSI::datashield.aggregate(.x, call("messageDS", test_obj_name)))
}

# Function to check for errors in studyside messages
check_studyside_errors <- function(studyside_messages) {
  studyside_messages %>%
    map_lgl(~. == "ALL OK: there are no studysideMessage(s) on this datasource") %>%
    all()
}

# Function to generate validity check message
generate_validity_check_message <- function(obj_name, no_errors) {
  if (no_errors) {
    validity_check <- paste0("<", obj_name, "> appears valid in all sources")
  } else {
    validity_check <- paste0("<", obj_name, "> invalid in at least one source. See studyside.messages:")
  }
  return(validity_check)
}

# Main function to perform all checks
.check_object_creation <- function(datasources, newobj) {
  object_info <- .get_obj_info(datasources, newobj)
  obj_exists <- check_obj_existence(object_info)
  obj_valid <- check_obj_validity(object_info)
  return_messages <- generate_return_messages(newobj, obj_exists, obj_valid)
  studyside_messages <- call_messageDS(datasources, newobj)
  no_errors <- check_studyside_errors(studyside_messages)
  validity_check <- generate_validity_check_message(newobj, no_errors)

  if (no_errors) {
    return(
      list(
        success = TRUE,
        messages = c(return_messages, validity_check)
    )
    )
  } else {
    return(
      list(
        sucess = FALSE,
        messages = return_messages, validity_check, studyside_messages
        )
    )
  }
}
#
# # Usage example
# datasources <- list("source1", "source2", "source3")
# test_obj_name <- "newobj"
# result <- .check_object_creation(datasources, newobj)
# print(result)






#'
#' @title Checks if the objects are defined in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be defined (i.e. exists)
#' in all the studies. If not the process should halt.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified, the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param obj a character vector, the name of the object(s) to look for.
#' @param error.message a Boolean which specifies if the function should stop and return
#' an error message when the input object is not defined in one or more studies or to
#' return a list of TRUE/FALSE indicating in which studies the object is defined
#' @keywords internal
#' @return returns an error message if \code{error.message} argument is set to TRUE (default)
#' and if the input object is not defined in one or more studies, or a Boolean value if
#' @importFrom DSI datashield.aggregate
#' \code{error.message} argument is set to FALSE.
#' @author Demetris Avraam for DataSHIELD Development Team
#'
isDefined <- function(datasources = NULL, obj = NULL, error.message = TRUE) {
  inputobj <- unlist(obj)

  for (i in 1:length(inputobj)) {
    extractObj <- extract(inputobj[i])

    if (is.na(extractObj$holders)) {
      cally <- call("exists", extractObj$elements)
      out <- DSI::datashield.aggregate(datasources, cally)
    } else {
      dfname <- as.name(extractObj$holders)
      cally <- call("exists", extractObj$elements, dfname)
      out <- DSI::datashield.aggregate(datasources, cally)
    }

    if (error.message == TRUE & any(out == FALSE)) {
      stop("The input object ", inputobj[i], " is not defined in ", paste(names(which(out == FALSE)), collapse = ", "), "!", call. = FALSE)
    } else {
      return(out)
    }
  }
}

#'
#' @title Checks an object has been generated on the server side
#' @description This is an internal function.
#' @details After calling an assign function it is important
#' to know whether or not the action has been completed by
#' checking if the output actually exists on the server side.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @param newobj a character, the name the object to look for.
#' @keywords internal
#' @return nothing is return but the process is stopped if
#' the object was not generated in any one server.
#' @importFrom DSI datashield.aggregate
#'
isAssigned <- function(datasources = NULL, newobj = NULL) {
  cally <- call("exists", newobj)
  qc <- DSI::datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc == TRUE))
  if (length(indx) > 0 & length(indx) < length(datasources)) {
    stop("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!", call. = FALSE)
  }
  if (length(indx) == 0) {
    stop("The output object has not been generated for any of the studies!", call. = FALSE)
  }
}

#'
#' @title Checks that an object has the same class in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be of the same type in all
#' the collaborating studies. If that is not the case the process is stopped
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @param obj a string character, the name of the object to check for.
#' @keywords internal
#' @return a message or the class of the object if the object has the same class in all studies.
#' @importFrom DSI datashield.aggregate
#'
#'
checkClass <- function(datasources = NULL, obj = NULL) {
  # check the class of the input object
  cally <- call("classDS", obj)
  classesBy <- DSI::datashield.aggregate(datasources, cally, async = FALSE)
  classes <- unique(unlist(classesBy))
  for (n in names(classesBy)) {
    if (!all(classes == classesBy[[n]])) {
      message("The input data is not of the same class in all studies!")
      message("Use the function 'ds.class' to verify the class of the input object in each study.")
      stop(" End of process!", call. = FALSE)
    }
  }
  return(classes)
}

#'
#' @title Splits character by '$' and returns the single characters
#' @description This is an internal function.
#' @details Not required
#' @param input a vector or a list of characters
#' @keywords internal
#' @return a vector of characters
#'
extract <- function(input) {
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)) {
    inputterms <- unlist(strsplit(input[i], "\\$", perl = TRUE))
    if (length(inputterms) > 1) {
      obj1 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][2]
    } else {
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list("holders" = output1, "elements" = output2)
  return(output)
}
