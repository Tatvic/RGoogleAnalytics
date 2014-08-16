#' Returns the URI constructed from the parameter settings. This also
#' URI-encodes all the values in each query parameter.
#' 
#' @param query.builder Name of the Object of the Query Builder Class
#' 
#' @param token Token object containing the OAuth2.0 Authentication details
#' 
#' @keywords internal
#' @return
#'   A full URI that can be used with the Google Analytics API. 
ToUri <- function(query.builder,token) {
  
  query <- c("start.date"  = query.builder$start.date(),
             "end.date"    = query.builder$end.date(),
             "dimensions"  = query.builder$dimensions(),
             "metrics"     = query.builder$metrics(),
             "segment"     = query.builder$segment(),
             "sort"        = query.builder$sort(),
             "filters"     = query.builder$filters(),
             "max.results" = query.builder$max.results(),
             "start.index" = query.builder$start.index(),
             "table.id"    = query.builder$table.id(),
             "access_token" = token$credentials$access_token)
  
  uri <- "https://www.googleapis.com/analytics/v3/data/ga?"
  for (name in names(query)) {
    uri.name <- switch(name,
                       start.date  = "start-date",
                       end.date    = "end-date",
                       dimensions  = "dimensions",
                       metrics     = "metrics",
                       segment     = "segment",
                       sort        = "sort",
                       filters     = "filters",
                       max.results = "max-results",
                       start.index = "start-index",
                       table.id    = "ids",
                       access_token = "access_token")
    
    if (!is.null(uri.name)) {
      uri <- paste(uri,
                   uri.name,
                   "=",
                   query[[name]],
                   "&",
                   sep = "",
                   collapse = "")
    }
  }
  # remove the last '&' that joins the query parameters together.
  uri <- sub("&$", "", uri)
  # remove any spaces that got added in from bad input.
  uri <- gsub("\\s", "", uri)
  return(uri)
}