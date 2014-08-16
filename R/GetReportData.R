#' Query the Google Analytics API for the specified dimensions, metrics and other query parameters
#' 
#' This function will retrieve the data by firing the query to the Core Reporting API. It also displays 
#' status messages after the completion of the query. The user also has the option split the query into 
#' daywise partitions and paginate the query responses in order to decrease the effect the sampling
#' @export
#' 
#' @param query.builder Name of the object created using \code{\link{QueryBuilder}}
#' 
#' @param token Name of the token object created using \code{\link{Auth}}
#'   
#' @param paginate_query  Pages through chunks of results by requesting maximum 
#' number of allowed rows at a time. Note that
#' if this argument is set to True, queries will take more longer to complete and use
#' more Quota. For more on Google Analytics API Quota check 
#' \url{https://developers.google.com/analytics/devguides/reporting/core/v3/limits-quotas#core_reporting}
#' 
#' @param split_daywise  Splits the query by date range into sub queries of 
#' single days. Setting this 
#' argument to True automatically paginates through each daywise query. Note that
#' if this argument is set to True, queries will take more longer to complete and use
#' more Quota 
#' 
#' @examples
#' \dontrun{
#' # This example assumes that a token object is already created
#' 
#' # Create a list of Query Parameters
#' query.list <- Init(start.date = "2014-11-28",
#'                    end.date = "2014-12-04",
#'                    dimensions = "ga:date",
#'                    metrics = "ga:sessions,ga:pageviews",
#'                    max.results = 1000,
#'                    table.id = "ga:33093633")
#'
#' # Create the query object
#' ga.query <- QueryBuilder(query.list)
#'
#' # Fire the query to the Google Analytics API
#' ga.df <- GetReportData(query, oauth_token)
#' ga.df <- GetReportData(query, oauth_token, split_daywise=True)
#' ga.df <- GetReportData(query, oauth_token, paginate_query=True)
#' }
#'
#' @return dataframe containing the response from the Google Analytics API 
#' 
#' @seealso Prior to executing the query, as a good practice 
#' queries can be tested in the Google Analytics Query Feed Explorer at \url{http://ga-dev-tools.appspot.com/explorer/}

GetReportData <- function(query.builder, token, 
                          split_daywise = FALSE,
                          paginate_query = FALSE) { 
  
  # Add an if (exists) block here
  kMaxDefaultRows <- get("kMaxDefaultRows", envir=rga.environment)
  
  # We have used oauth 2.0 API to authorize the user account 
  # and to get the accesstoken to request to the Google Analytics Data API. 
  query.uri <- NULL
  dataframe.param <- data.frame()
  
  # Set the CURL options for Windows    
  options(RCurlOptions = list(capath = system.file("CurlSSL",
                                                   "cacert.pem", 
                                                   package = "RCurl"),
                              ssl.verifypeer = FALSE))
  
  
  # Set all the Query Parameters
  
  query.builder$SetQueryParams()
  # query.builder$Validate()
  
  # Ensure the starting index is set per the user request
  # We can only return 10,000 rows in a single query
  # kMaxDefaultRows <- 10000
  max.rows <- query.builder$max.results()
  
  # If the user does not require pagination and query splitting 
  # fire the query and display the status messages
  if (split_daywise != T && paginate_query != T) {
    query.uri <- ToUri(query.builder,token)
    ga.list <- GetDataFeed(query.uri)
    
    total.results <-  ga.list$totalResults
    items.per.page <- ga.list$itemsPerPage
    contains.sampled.data <- ga.list$containsSampledData
    response.size <- length(ga.list$rows)
    
    if (total.results < kMaxDefaultRows) {
      max.rows <- kMaxDefaultRows
    }
    
    # Convert the list object to a dataframe
    if (length(query.builder$dimensions()) == 0) {
      totalrows <- 1
      dataframe.param <- ga.list$rows[[1]]
      dim(dataframe.param) <- c(1, length(dataframe.param))
    } else {
      totalrows <- nrow(do.call(rbind, as.list(ga.list$rows)))
      dataframe.param <- rbind(dataframe.param, 
                               do.call(rbind, as.list(ga.list$rows)))
    }
    
    final.df <- SetDataFrame(ga.list$columnHeaders, dataframe.param)
    
    # Print the status messages if query is not in batch mode
    if (length(ga.list$rows) < total.results) {
      cat("Status of Query:\n")
      cat("The API returned", response.size, "results out of", total.results, "results\n")
      cat("In order to get all results, set paginate_query = T in the GetReportData function.\n")
    } else {
      cat("Status of Query:\n")
      cat("The API returned", response.size, "results\n")
    }
    
    # Calculate the Percentage of Visits based on which the query was sampled
    # Reference : https://developers.google.com/analytics/devguides/reporting/core/v3/reference#sampling
    if (contains.sampled.data == T) {
      visits.for.sampled.query <- round(100 * (as.integer(ga.list$sampleSize) /
                                                 as.integer(ga.list$sampleSpace)),2)
      cat("The query response contains sampled data. It is based on ", visits.for.sampled.query, "% of your visits.\n")
      cat("You can split the query day-wise in order to reduce the effect of sampling.\n")
      cat("Set split_daywise = T in the GetReportData function\n")
      cat("Note that split_daywise = T will automatically invoke Pagination in each sub-query\n")
    }
  } else if ((split_daywise == T) || (split_daywise == T && paginate_query == T)) {
    
    # Clamp Max Results to kMaxDefaultRows while Query Splitting
    # Implement this via SetMaxResults() in future versions
    if (query.builder$max.results() < kMaxDefaultRows) {
      cat("Setting Max Results to 10000 for efficient Query Utilization\n")
      query.builder$max.results(kMaxDefaultRows)
    }
    GA.DF <- SplitQueryDaywise(query.builder, token)
    final.df <- SetDataFrame(GA.DF$header, GA.DF$data)
    cat("The API returned", nrow(final.df), "results\n")
    
  } else if (paginate_query == T) {
    
    # Clamp the Max Results parameter to 10000 for efficient query utilization 
    # when paginating
    # Implement SetMaxResults() as a method in QueryBuilder()
    if (query.builder$max.results() < kMaxDefaultRows) {
      cat("Setting Max Results to 10000 for efficient Query Utilization\n")
      query.builder$max.results(kMaxDefaultRows)    
    }
    
    # Hit One Query
    query.uri <- ToUri(query.builder, token)
    ga.list <- GetDataFeed(query.uri)
    # Convert ga.list into a dataframe
    ga.list.df <- data.frame()
    ga.list.df <- rbind(ga.list.df, do.call(rbind, as.list(ga.list$rows)))
    
    # Check if pagination is required
    
    if (length(ga.list$rows) < ga.list$totalResults) {
      number.of.pages <- ceiling(ga.list$totalResults / length(ga.list$rows))
      
      # Clamp Number of Pages to 100 in order to enforce upper limit for pagination as 1M rows
      if (number.of.pages > 100) {
        number.of.pages <- get("kMaxPages", envir=rga.environment)
      }
      
      # Call Pagination Function
      paged.query.list <- PaginateQuery(query.builder, number.of.pages, token)
      
      # Collate Results and convert to Dataframe
      inter.df <- rbind(ga.list.df, paged.query.list$data)
      final.df <- SetDataFrame(paged.query.list$headers, inter.df)
      
      cat("The API returned", nrow(final.df), "results\n")
    } else {
      stop("Pagination is not required. Set paginate_Query = F and re-run the query\n")
    } 
  }
  return(final.df)
}