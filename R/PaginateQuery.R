#' @title
#' Paginate through pages of Google Analytics Query responses
#' 
#' In case if a single query returns more than 10k rows,the Core Reporting API returns a subset of
#' the rows at a time. This function loops across all such subsets (pages) in order to retrieve data corresponding
#' to the entire query. The maximum number of rows corresponding to a single query that can be retrieved via Pagination
#' is 1 M. 
#' 
#' @seealso https://developers.google.com/analytics/devguides/reporting/core/v2/gdataReferenceDataFeed#largeDataResults
#' 
#' @keywords internal 
#' 
#' @param query.builder Name of the object corresponding to the query builder class
#' 
#' @param pages Integer representing the number of pages across which the query has to be paginated
#' 
#' @param token Token Object created by \code{Auth()} 
#' 
#' @return list containing Column Headers and the data collated across all the pages of the query
#' 
#' 
PaginateQuery <- function(query.builder, pages, token) {
  
  kMaxDefaultRows <- get("kMaxDefaultRows", envir=rga.environment)
  
  # Validate the token and regenerate it if expired
  ValidateToken(token)
  
  # Create an empty dataframe in order to store the data
  df.inner <- data.frame()
  
  for (i in (1:(pages - 1))) {
    dataframe.param <- data.frame()
    start.index <- (i * kMaxDefaultRows) + 1
    cat("Getting data starting at row", start.index, "\n")
    query.builder$SetStartIndex(start.index)
    query.uri <- ToUri(query.builder, token)
    ga.list <- GetDataFeed(query.uri)
    dataframe.param <- rbind(dataframe.param,
                             do.call(rbind, as.list(ga.list$rows)))
    df.inner <- rbind(df.inner, dataframe.param)
    col.headers <- ga.list$columnHeaders
    rm(ga.list)
  }
  
  return(list(headers=col.headers, data=df.inner))
}