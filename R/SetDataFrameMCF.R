#' To prepare the dataframe by applying the column names and column datatypes 
#' to the provided data frame.
#' @keywords internal  
#' @param GA.list.param.columnHeaders list includes GA response data, column name, column datatype.
#' @param dataframe.param The reponse data(dimensions and metrics data rows) packed in dataframe without the column names and column types.
#' @return dataframe.param The dataframe attached with the column  names and column datatype as per type the Dimensions and metrics
#'
SetDataFrameMCF <- function(GA.list.param.columnHeaders, dataframe.param) {
  
  
  column.param <- t(sapply(GA.list.param.columnHeaders, 
                           '[',
                           1 : max(sapply(GA.list.param.columnHeaders,
                                          length))))
  col.name <- gsub('mcf:', '', as.character(column.param[, 1]))#Set to mcf
  col.datatype <- as.character(column.param[, 3])
  colnames(dataframe.param) <- col.name
  
  dataframe.param <- as.data.frame(dataframe.param)
  #dataframe.param <- SetColDataType(col.datatype, col.name, dataframe.param) #produces NAs
  
  return(dataframe.param)
}