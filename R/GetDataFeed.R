#' This will request with the prepared Query to the Google Analytics 
#' Data feed API and returns the data in dataframe R object.
#' 
#' @keywords internal 
#' 
#' @param query.uri The URI prepared by the QueryBuilder class.   
#' @param caching.dir String Directory to save cached data
#' @param caching Boolean caching required?
#' 
#' @return GA.list The Google Analytics API JSON response converted to a 
#' list object
#' 
#' @importFrom httr GET
#' @importFrom digest digest
GetDataFeed <- function(query.uri, caching.dir = NULL, caching = FALSE) {
  if (caching == TRUE) {
    uri_without_token <- gsub("access_token=[^\\&]*", "", query.uri)
    hash <- digest::digest(uri_without_token)
    filename <- paste0(caching.dir,"/cache", "-", hash, ".Rda")
    if (file.exists(filename)){
      load(filename)
    }else{
      GA.Data <- GET(query.uri)  
      save(GA.Data, file=filename)
    }
  } else {
    GA.Data <- GET(query.uri)  
  }
  
  
  GA.list <- ParseDataFeedJSON(GA.Data)
  if (is.null(GA.list$rows)) {
    warning("Your query matched 0 results. Please verify your query using the Query Feed Explorer and re-run it.")
    return(NULL)
    # break
  } else {
    return (GA.list)
  }
}