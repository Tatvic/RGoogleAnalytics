#' Queries the Google Analytics API for the specified dimensions,metrics and other query parameters
#' 
#' This function will retrieve the data by firing the query to the Core Reporting API. It also displays 
#' status messages after the completion of the query. The user also has the option split the query into 
#' daywise partitions and paginate the query responses in order to decrease the effect the sampling
#' @export
#' 
#' @param query.builder  Name of the object corresponding to the QueryBuilder class 
#' 
#' @param paginate_query  Pages through chunks of results by requesting maximum 
#' number of allowed rows at a time. Default Value of this argument is FALSE
#' 
#' @param split_daywise  Splits the query by date range into sub queries of 
#' single days. Default value of this argument is FALSE. Note that setting this 
#' argument to true automatically paginates through each daywise query 
#' 
#' @examples
#' \dontrun{ga_df <- GetReportData(query)
#' ga_df <- GetReportData(query,split_daywise=True)
#' ga_df <- GetReportData(query,paginate_query=True)
#' }
#' @return api.response The respose is in the dataframe format as the output data returned from the Google Analytics Data feed API.
#' 

GetReportData <- function(query.builder, 
                          split_daywise=FALSE,
                          paginate_query=FALSE) { 
                     
  kMaxDefaultRows <- 10000 
  kMaxPages <- 100
  
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
    ga.list <- GetDataFeed(query.builder$to.uri())
    
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
      if (max.rows < kMaxDefaultRows) {
        cat("Set max.rows = 10000 for efficient query utilization while Paginating\n")
      }
    } else {
      cat("Status of Query:\n")
      cat("The API returned", response.size, "results")
    }
    
    # Calculate the Percentage of Visits based on which the query was sampled
    # Reference : https://developers.google.com/analytics/devguides/reporting/core/v3/reference#sampling
    if (contains.sampled.data == T) {
      visits.for.sampled.query <- round(100 * (as.integer(ga.list$sampleSize)/
                                                 as.integer(ga.list$sampleSpace)),2)
      cat("The query response contains sampled data. It is based on ", visits.for.sampled.query, "% of your visits.\n")
      cat("You can split the query day-wise in order to reduce the effect of sampling.\n")
      cat("Set split_daywise = T in the GetReportData function\n")
      cat("Note that split_daywise = T will automatically invoke Pagination in each sub-query\n")
    }
  } else if (split_daywise == T) {
    
    GA.DF <- SplitQueryDaywise(query.builder)
    final.df <- SetDataFrame(GA.DF$header,GA.DF$data)
    cat("The API returned", nrow(final.df), "results\n")
    
  } else if (paginate_query == T) {
    
    # Validate the token and regenerate it if expired
    #ValidateToken()
    
    #load(file.path(path.package("RGoogleAnalytics"),
    #"accesstoken.rda"))
    
    #Update the access token in the query object
    #query.builder$SetAccessToken(token.list$access_token)
    
    # Hit One Query
    ga.list <- GetDataFeed(query.builder$to.uri())
    # Convert ga.list into a dataframe
    ga.list.df <- data.frame()
    ga.list.df <- rbind(ga.list.df, do.call(rbind,as.list(ga.list$rows)))
    
    # Check if pagination is required
    
    if (length(ga.list$rows) < ga.list$totalResults) {
      number.of.pages <- ceiling(ga.list$totalResults/length(ga.list$rows))
      
      # Clamp Number of Pages to 100 in order to enforce upper limit for pagination as 1M rows
      if (number.of.pages > 100) {
        number.of.pages <- kMaxPages
      }
      
      # Call Pagination Function
      paged.query.list <- PaginateQuery(query.builder, number.of.pages)
      
      # Collate Results and convert to Dataframe
      inter.df <- rbind(ga.list.df,paged.query.list$data)
      final.df <- SetDataFrame(paged.query.list$headers, inter.df)
      
      cat("The API returned", nrow(final.df), "results\n")
    } else {
      stop("Pagination is not required in the query.Set Paginate_Query = F and re-run the query\n")
    }
  }
  return(final.df)
}