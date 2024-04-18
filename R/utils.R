.getEncodeKey <- function(){

  encode_list <- list(
      input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|"),
      output = c("$LIST$", "$LB", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$")
    )
  return(encode_list)
}

.encodeTidyEval <- function(input_string, encode_key){
  encode_vec <- setNames(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}






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
isDefined <- function(datasources=NULL, obj=NULL, error.message=TRUE){

  inputobj <- unlist(obj)

  for(i in 1:length(inputobj)){

    extractObj <- extract(inputobj[i])

    if(is.na(extractObj$holders)){
      cally <- call('exists', extractObj$elements)
      out <- DSI::datashield.aggregate(datasources, cally)
    }else{
      dfname <- as.name(extractObj$holders)
      cally <- call('exists', extractObj$elements, dfname)
      out <- DSI::datashield.aggregate(datasources, cally)
    }

    if(error.message==TRUE & any(out==FALSE)){
      stop("The input object ", inputobj[i], " is not defined in ", paste(names(which(out==FALSE)), collapse=", "), "!" , call.=FALSE)
    }else{
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
isAssigned <- function(datasources=NULL, newobj=NULL){
  cally <- call('exists', newobj)
  qc <- DSI::datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  if(length(indx) > 0 & length(indx) < length(datasources)){
    stop("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!", call.=FALSE)
  }
  if(length(indx) == 0){
    stop("The output object has not been generated for any of the studies!", call.=FALSE)
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
checkClass <- function(datasources=NULL, obj=NULL){
  # check the class of the input object
  cally <- call("classDS", obj)
  classesBy <- DSI::datashield.aggregate(datasources, cally, async = FALSE)
  classes <- unique(unlist(classesBy))
  for (n in names(classesBy)) {
    if (!all(classes == classesBy[[n]])) {
      message("The input data is not of the same class in all studies!")
      message("Use the function 'ds.class' to verify the class of the input object in each study.")
      stop(" End of process!", call.=FALSE)
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
extract <- function(input){
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)){
    inputterms <- unlist(strsplit(input[i], "\\$", perl=TRUE))
    if(length(inputterms) > 1){
      obj1 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][2]
    }else{
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list('holders'=output1, 'elements'=output2)
  return(output)
}
