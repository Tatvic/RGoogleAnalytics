#' The main builder class for constructing URI requests.
#' 
#' This function lists all the elements and parameters that make up a data
#' feed request. In general, you provide the profile ID corresponding to the
#' profile you want to retrieve data from, choose the combination of
#' dimensions and metrics, and provide a date range along with other
#' parameters in a query string.
#'
#' @export
#' @param start.date Start Date for fetching Analytics Data.
#' Start Date must be of the format "\%Y-\%m-\%d"
#' 
#' @param end.date End Date for fetching Analytics Data.
#' End Date must be of the format "\%Y-\%m-\%d"
#' 
#' @param dimensions A list of comma separated dimensions for Analytics Data
#' 
#' @param metrics A list of comma separated metrics for Analytics Data
#' 
#' @param sort A list of comma separated metrics and dimensions for sorting the Analytics
#' data and the sorting direction for these dimensions/metrics
#' 
#' @param filters Dimensions and metrics filters that restrict the data for a request
#' 
#' @param segments Segments the data for your request
#' 
#' @param max.results Maximum Number of rows to include in the query response. Default value is 
#' 10000
#' 
#' @param table.id Profile ID of the form ga:XXXXX where XXXXX is the Analytics View (Profile) ID of 
#' for which the query will retrieve the data
#' 
#' @return query.params.list List of all the Query Parameters initialized by the user

Init <- function(
  start.date = NULL,
  end.date = NULL,
  dimensions = NULL,
  metrics = NULL,
  filters = NULL,
  sort = NULL
  segments = NULL,
  max.results = NULL,
  start.index = NULL,
  table.id = NULL){
  
  query.params.list = list("start.date" = start.date,
                 "end.date" = end.date,
                 "dimensions" = dimensions,
                 "metrics" = metrics,
                 "filters" = filters,
                 "sort" = sort,
                 "segments" = segments,
                 "max.results" = max.results,
                 "start.index" = start.index,
                 "table.id" = table.id)
  
  return(query.params.list)
}
