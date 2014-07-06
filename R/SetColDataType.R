#' This will set the appropriate data type to the each column of the 
#' provided dataframe.
#' @keywords internal 
#' @param col.datatype The vector of the datatype of all the dimensions and metrics from the parsed list data.
#' @param col.name The vector of the name of the all dimensions and metrics of the retrived data and it will be attached to the dataframe.param.               
#' @param dataframe.param 
#'              
#' @return dataframe.param The dataframe will be set with its column names with the appropriate class type.
#' 
#' 
SetColDataType <- function(col.datatype, col.name, dataframe.param) {
        
  for(i in 1:length(col.datatype)) {
    if (col.datatype[i] == "STRING") {
      dataframe.param[, i] <- as.character(dataframe.param[, i]) 
    } else {
      dataframe.param[, i] <- as.numeric(as.character(dataframe.param[, i])) 
    }
  }
  return(dataframe.param)
}