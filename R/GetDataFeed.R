#' This will request with the prepared Query to the Google Analytics 
#' Data feed API and returns the data in dataframe R object.
#' 
#' @keywords internal 
#' 
#' @param query.uri The URI prepared by the QueryBuilder class.   
#' 
#' @return GA.list The Google Analytics API JSON response converted to a list object
GetDataFeed <- function(query.uri) {
  
  GA.Data <- getURL(query.uri)  
  GA.list <- ParseDataFeedJSON(GA.Data)
  if (is.null(GA.list$rows)) {
    cat("Your query matched 0 results. Please verify your query.")
    break
  } else {
    return (GA.list)
  }
}