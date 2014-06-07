#' To prepare the dataframe by applying the column names and column datatypes 
#' to the provided data frame.
#' @keywords internal  
#' @param GA.list.param.columnHeaders list includes GA response data, column name, column datatype.
#' @param dataframe.param The reponse data(dimensions and metrics data rows) packed in dataframe without the column names and column types.
#' @return dataframe.param The dataframe attached with the column  names and column datatype as per type the Dimensions and metrics
#'
SetDataFrame <- function(GA.list.param.columnHeaders, dataframe.param) {
          
  
  column.param <- t(sapply(GA.list.param.columnHeaders, 
                           '[',
                           1 : max(sapply(GA.list.param.columnHeaders,
                                          length))))
  col.name <- gsub('ga:', '', as.character(column.param[, 1]))
  col.datatype <- as.character(column.param[, 3])
  colnames(dataframe.param) <- col.name
  
  dataframe.param <- as.data.frame(dataframe.param)
  dataframe.param <- SetColDataType(col.datatype, col.name, dataframe.param)
  
  return(dataframe.param)
}