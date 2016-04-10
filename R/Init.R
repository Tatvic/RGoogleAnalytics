#' Initialize the Google Analytics query parameters
#' 
#' This function takes all the query parameters and combines them into a single list that 
#' is to be passed as an argument to \code{\link{QueryBuilder}}. Note that parameter validation is 
#' performed when the \code{\link{QueryBuilder}} object is created
#'
#' @export
#' @param start.date Start Date for fetching Analytics Data.
#' Start Date must be of the format "\%Y-\%m-\%d"
#' 
#' @param end.date End Date for fetching Analytics Data.
#' End Date must be of the format "\%Y-\%m-\%d"
#' 
#' @param dimensions Optional. A vector of up to 7 dimensions, either as a single string or a vector or strings, E.g.
#'                  "ga:source,ga:medium" or c("ga:source", "ga:medium").      
#'                      
#' @param metrics A vector of up to 10 metrics, either as a single string or a vector or strings. E.g.
#'                "ga:sessions" or c("ga:sessions", "ga:bounces").  
#' 
#' @param sort Optional.The sorting order for the data to be returned.e.g. "ga:sessions" or c("ga:sessions", "-ga:browser") 
#' 
#' @param filters Optional.The filter string for the GA request.e.g. "ga:medium==referral".
#' 
#' @param segments Optional.An advanced segment definition to slice and dice your
#'            Analytics data. 
#' 
#' @param max.results Optional.Maximum Number of rows to include in the query response. Default value is 
#' 10000
#' 
#' @param table.id Profile ID of the form ga:XXXXX where XXXXX is the Analytics View (Profile) ID of 
#' for which the query will retrieve the data. The View ID can be found under View Settings by navigating 
#' to the Admin Tab under your Google Analytics Profile
#'  
#' @param start.index Optional.The first row of data to retrieve. Default value is 1
#' 
#' @param caching.dir String Directory to save cached data
#' 
#' @param caching Boolean caching required?
#' 
#' @seealso
#' Valid Combinations of Dimensions and Metrics can be found at  \url{http://code.google.com/apis/analytics/docs/gdata/gdataReferenceDimensionsMetrics.html#validCombinations}
#' 
#' 
#' @return List of all the Query Parameters initialized by the user

Init <- function(
  start.date = NULL,
  end.date = NULL,
  dimensions = NULL,
  metrics = NULL,
  filters = NULL,
  sort = NULL,
  segments = NULL,
  max.results = NULL,
  start.index = NULL,
  table.id = NULL,
  caching.dir = NULL,
  caching = FALSE){
  
  query.params.list = list("start.date" = start.date,
                 "end.date" = end.date,
                 "dimensions" = dimensions,
                 "metrics" = metrics,
                 "filters" = filters,
                 "sort" = sort,
                 "segments" = segments,
                 "max.results" = max.results,
                 "start.index" = start.index,
                 "table.id" = table.id,
                 "caching.dir" = caching.dir,
                 "caching" = caching)
  return(query.params.list)
}
