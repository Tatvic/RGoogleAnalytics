#' This function breaks up the time range (as specified by Start Date and End Date) into single days
#' and hits a query for each day. The responses for each queries are collated into a single dataframe and
#' returned. This procedure helps decrease the effect of sampling.
#' 
#' @seealso https://developers.google.com/analytics/devguides/reporting/core/v2/gdataReferenceDataFeed#largeDataResults
#' 
#' @keywords internal 
#' 
#' @param query.builder Name of the object corresponding to the QueryBuilder class
#' @param token Token Object created by the Auth() function   
#' @param delay Time delay in seconds between successive queries in order to avoid Rate Limit Error
#' 
#' @return list containing the Column Headers and the Collated dataframe that represents the query response 
#' 
#' @importFrom lubridate ymd
#' @importFrom lubridate days

SplitQueryDaywise <- function(query.builder, token, delay) {
     
  kMaxDefaultRows <- get("kMaxDefaultRows",envir = rga.environment)
  
  # Validate the token and regenerate it if expired
  ValidateToken(token)
  
  # Updated to use Get Methods
  start.date <- ymd(query.builder$GetStartDate())
  end.date   <- ymd(query.builder$GetEndDate())
  
  date.difference <- as.numeric(difftime(end.date, start.date, units = 'days'))
  
  if (date.difference == 0) {
    stop("Please verify start date and end date. They cannot be the same")
  }
  
  # Create an empty dataframe in order to store the results
  master.df <- data.frame()
  
  # Create an empty data.frame in order to store the headers
  return.df.header <- data.frame()
  
  for (i in (0:date.difference)) {
    # Update the start and end dates in the query
    date <- format(as.POSIXct(start.date) + days(i), '%Y-%m-%d')
    message("[ Run ", i, " of ", date.difference, "] Getting data for ", date)
    query.builder$SetStartDate(date)
    query.builder$SetEndDate(date)
    
    # Reset the start index to 1 since a new query will be fired for a different date-range
    query.builder$SetStartIndex(as.numeric(1))
    
    # Hit the first query corresponding the particular date
    first.query.df <- data.frame()
    inter.df <- data.frame()
    query.uri <- ToUri(query.builder, token)
    Sys.sleep(delay)
    first.query <- GetDataFeed(query.uri, caching.dir = query.builder$caching.dir, caching = query.builder$caching)
  
    # Save old header in case there is one
    if(!is.null(first.query$columnHeaders)){
      return.df.header <- first.query$columnHeaders
    }    
      
    if (!is.null(first.query)){
      first.query.df <- rbind(first.query.df, do.call(rbind, as.list(first.query$rows)))
    
      # Check if pagination is required in the query
    
      if (length(first.query$rows) < first.query$totalResults) {
        number.of.pages <- ceiling((first.query$totalResults) / length(first.query$rows))
          if ((number.of.pages > 100) & exists("kMaxPages", envir = rga.environment))  {
            number.of.pages <- get("kMaxPages", envir = rga.environment)
        }
        inter.df <- PaginateQuery(query.builder, number.of.pages, token, delay)
        inter.df <- rbind(first.query.df, inter.df$data)
        master.df <- rbind(master.df, inter.df)
      } else {
        # No Pagination is required. Just append the rows to the dataframe
        master.df <- rbind(first.query.df, master.df)
      }
    }
  }
  
  if(is.null(return.df.header)){
    warning("The API returned 0 rows.")
    return(NULL)
  }
  
  return(list(header = return.df.header, data=master.df))
}