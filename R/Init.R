#' A function setting initial values of a GA URI query.
#'
#' @export
#' @param start.date See QueryBuilder()
#' @param end.date See QueryBuilder()
#' @param dimensions See QueryBuilder()  
#' @param metrics See QueryBuilder() 
#' @param segment See QueryBuilder()  
#' @param sort See QueryBuilder() 
#' @param filters See QueryBuilder()
#' @param max.results See QueryBuilder()
#' @param start.index: See QueryBuilder()  
#' @param table.id: See QueryBuilder() 
#' @param access_token: See AccessToken() 
#'  
#'
#' @return None Sets the initial query parameters.
#'

Init <- function(start.date  = NULL,
                 end.date    = NULL,
                 dimensions  = NULL,
                 metrics     = NULL,
                 segment     = NULL,
                 sort        = NULL,
                 filters     = NULL,
                 max.results = NULL,
                 start.index = NULL,
                 table.id    = NULL) {   
  
  #Load Access Token from Memory
  access_token <- LoadAccessToken()
  
  StartDate(start.date)
  EndDate(end.date)
  Dimensions(dimensions)
  Metrics(metrics)
  Segment(segment)
  Sort(sort)
  Filters(filters)
  MaxResults(max.results)
  StartIndex(start.index)
  TableID(table.id)
  AccessToken(access_token)
  
  #Perform Validation
  #Validate()
  return(invisible())
}