\name{QueryBuilder}
\alias{QueryBuilder}

\title{
Builds a query with the specified dimensions,metrics and other query parameter}
\description{
The main builder class for constructing URI requests.This function lists all the elements and parameters that make up a data feed request. In general, you provide the profile ID corresponding to the profile you want to retrieve data from, choose the combination of dimensions and metrics, and provide a date range along with other parameters in a query string.
}
\usage{
QueryBuilder()
query <- QueryBuilder()  
query$Init(start.date = "2012-06-18",
     end.date = "2012-12-18",
     dimensions = "ga:date, ga:pagePath",
     metrics = "ga:sessions, ga:pageviews, ga:timeOnPage",
     sort = "ga:sessions",
     max.results = 10000,
     table.id = 'ga:12345',
     )
}     
\details{

}
\value{
Returns a query object that can be then passed to the GetReportData function
}
\references{

}
\author{
Michael Pearmain, Nick Mihailowski
}
\note{
}
\seealso{
}
\examples{
## The function is currently defined as
function () 
{
    kMaxDimensions <- 7
    kMaxMetrics <- 10
    kMaxTableIds <- 1
    start.date <- NULL
    end.date <- NULL
    dimensions <- NULL
    metrics <- NULL
    segment <- NULL
    sort <- NULL
    filters <- NULL
    max.results <- NULL
    start.index <- NULL
    profile.id <- NULL
    access_token <- NULL
    StartDate <- function(start.date.param = NA) {
        if (is.null(start.date.param)) {
            start.date <<- NULL
            return(invisible())
        }
        if (is.na(start.date.param)) {
            return(start.date)
        }
        if (is.na(as.Date(start.date.param, "\%Y-\%m-\%d"))) {
            stop("A start date must be specified of the form YYYY-MM-DD")
        }
        start.date <<- start.date.param
        return(invisible())
    }
    EndDate <- function(end.date.param = NA) {
        if (is.null(end.date.param)) {
            end.date <<- NULL
            return(invisible())
        }
        if (is.na(end.date.param)) {
            return(end.date)
        }
        if (is.na(as.Date(end.date.param, "\%Y-\%m-\%d"))) {
            stop("An end date must be specified of the form YYYY-MM-DD")
        }
        end.date <<- end.date.param
        return(invisible())
    }
    Dimensions <- function(dimensions.param = NA) {
        if (is.null(dimensions.param)) {
            dimensions <<- NULL
            return(invisible())
        }
        if (is.na(dimensions.param[1])) {
            return(dimensions)
        }
        if (!is.vector(dimensions.param)) {
            stop(paste("dimensions must be a vector of string variables"))
        }
        if (length(dimensions.param) > kMaxDimensions) {
            stop(paste("Google Analytics can only handle up to", 
                kMaxDimensions, "dimensions parameters"))
        }
        if (!is.character(dimensions.param)) {
            stop(paste("dimensions must be character, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        dimensions <<- paste(dimensions.param, collapse = ",")
        return(invisible())
    }
    Metrics <- function(metrics.param = NA) {
        if (is.null(metrics.param)) {
            metrics <<- NULL
            return(invisible())
        }
        if (is.na(metrics.param[1])) {
            return(metrics)
        }
        if (!is.vector(metrics.param)) {
            stop("metrics must be a vector of string variables")
        }
        if (length(metrics.param) > kMaxMetrics) {
            stop(paste("Google Analytics can only handle up to", 
                kMaxMetrics, "metrics parameters"))
        }
        if (!is.character(metrics.param)) {
            stop(paste("metrics must be character string, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        metrics <<- paste(metrics.param, collapse = ",")
        return(invisible())
    }
    Segment <- function(segment.param = NA) {
        if (is.null(segment.param)) {
            segment <<- NULL
            return(invisible())
        }
        if (is.na(segment.param[1])) {
            return(segment)
        }
        segment <<- segment.param
        return(invisible())
    }
    Sort <- function(sort.param = NA) {
        if (is.null(sort.param)) {
            sort <<- NULL
            return(invisible())
        }
        if (is.na(sort.param[1])) {
            return(sort)
        }
        if (!is.vector(sort.param)) {
            stop("sort must be a vector of string variables")
        }
        if (!is.character(sort.param)) {
            stop(paste("sort must be character string, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        sort <<- paste(sort.param, collapse = ",")
        return(invisible())
    }
    Filters <- function(filters.param = NA) {
        if (is.null(filters.param)) {
            filters <<- NULL
            return(invisible())
        }
        if (is.na(filters.param[1])) {
            return(filters)
        }
        filters <<- filters.param
        return(invisible())
    }
    MaxResults <- function(max.results.param = NA) {
        if (is.null(max.results.param)) {
            max.results <<- NULL
            return(invisible())
        }
        if (is.na(max.results.param[1])) {
            return(max.results)
        }
        if (!is.numeric(max.results.param)) {
            stop("max.results must be a number")
        }
        check.vector.length <- length(max.results.param)
        if (check.vector.length > 1) {
            stop("Max Results must be a single numeric value")
        }
        max.results <<- max.results.param
        return(invisible())
    }
    StartIndex <- function(start.index.param = NA) {
        if (is.null(start.index.param)) {
            start.index <<- NULL
            return(invisible())
        }
        if (is.na(start.index.param[1])) {
            return(start.index)
        }
        if (!is.numeric(start.index.param)) {
            stop("start.index must be a number")
        }
        check.vector.length <- length(start.index.param)
        if (check.vector.length > 1) {
            stop("Start index must be a single numeric value")
        }
        start.index <<- start.index.param
        return(invisible())
    }
    TableID <- function(table.id.param = NA) {
        if (is.null(table.id.param)) {
            table.id <<- NULL
            return(invisible())
        }
        if (is.na(table.id.param[1])) {
            return(table.id)
        }
        if (!is.character(table.id.param)) {
            stop("A table.id  must be of the a character of the form ga:XXXX")
        }
        if (!is.vector(table.id.param)) {
            stop(paste("table.id must be a vector (length ", 
                kMaxTableIds, ") string variable"))
        }
        if (length(table.id.param) != kMaxTableIds) {
            stop(paste("Only", kMaxTableIds, "table.id can be used at a time."))
        }
        table.id <<- table.id.param
        return(invisible())
    }
    Validate <- function() {
        missing.params <- c()
        if (is.null(start.date)) {
            missing.params <- append(missing.params, "start.date")
        }
        if (is.null(end.date)) {
            missing.params <- append(missing.params, "end.date")
        }
        if (is.null(metrics)) {
            missing.params <- append(missing.params, "metrics")
        }
        if (is.null(table.id)) {
            missing.params <- append(missing.params, "table.id")
        }
        if (is.null(access_token)) {
            missing.params <- append(missing.params, "access_token")
        }
        if (length(missing.params) == 0) {
            return(TRUE)
        }
        missing.string <- paste(missing.params, collapse = ", ")
        stop(paste("All GA queries must have", missing.string, 
            "parameters.", sep = " "))
        if ((as.numeric(difftime(ymd(start.date), ymd(end.date), 
            units = "days"))) < 0) {
            stop("Start Date must precede End date. Please correct the dates and re-initialize the query")
        }
    }
    ToUri <- function() {
        query <- c(start.date = start.date, end.date = end.date, 
            dimensions = dimensions, metrics = metrics, segment = segment, 
            sort = sort, filters = filters, max.results = max.results, 
            start.index = start.index, table.id = table.id, access_token = access_token)
        uri <- "https://www.googleapis.com/analytics/v3/data/ga?"
        for (name in names(query)) {
            uri.name <- switch(name, start.date = "start-date", 
                end.date = "end-date", dimensions = "dimensions", 
                metrics = "metrics", segment = "segment", sort = "sort", 
                filters = "filters", max.results = "max-results", 
                start.index = "start-index", table.id = "ids", 
                access_token = "access_token")
            if (!is.null(uri.name)) {
                uri <- paste(uri, uri.name, "=", curlEscape(query[[name]]), 
                  "&", sep = "", collapse = "")
            }
        }
        uri <- sub("&$", "", uri)
        uri <- gsub("\\s", "", uri)
        return(uri)
    }
    ClearData <- function() {
        start.date <<- NULL
        end.date <<- NULL
        dimensions <<- NULL
        metrics <<- NULL
        segment <<- NULL
        sort <<- NULL
        filters <<- NULL
        max.results <<- NULL
        start.index <<- NULL
        table.id <<- NULL
        return(invisible())
    }
    AccessToken <- function(access_token.param = NULL) {
        if (is.null(access_token.param)) {
            access_token <<- NULL
            return(invisible())
        }
        if (is.na(access_token.param)) {
            return(access_token)
        }
        if (!is.character(access_token.param)) {
            stop("not in character")
        }
        if (!is.vector(access_token.param)) {
            stop(paste("access_token must be a vector"))
        }
        access_token <<- access_token.param
        return(invisible())
    }
    Init <- function(start.date = NULL, end.date = NULL, dimensions = NULL, 
        metrics = NULL, segment = NULL, sort = NULL, filters = NULL, 
        max.results = NULL, start.index = NULL, table.id = NULL) {
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
        return(invisible())
    }
    Authorize <- function() {
        browseURL(paste("https://accounts.google.com/o/oauth2/auth?scope=", 
            "https://www.googleapis.com/auth/analytics.readonly&", 
            "response_type=code&access_type=offline&redirect_uri=", 
            "https://developers.google.com/oauthplayground&approval", 
            "_prompt=force&client_id=407408718192.apps.", "googleusercontent.com&hl=en&from_login=1&as=", 
            "7886e0e26859b9a5&pli=1&authuser=0", sep = ""))
        cat("The GA data extraction process required access token.", 
            "To accept the accesstoken from Oauth 2.0 Playground, you need", 
            "to follow certain steps in your browser. This access token will be", 
            "valid untill an hour only.\n\nSteps to be followed : \n1. Authorize your", 
            "Google Analytics account by providing email and password. \n2. On left", 
            "side of the scrren click on the button", "'Exchange authorization code for", 
            "tokens' to generate the access token. \n3. Copy the generated access", 
            "token and paste it here.")
        access_token <- readline(as.character(cat("\n\nPaste the access token here", 
            ":=>")))
        return(access_token)
    }
    SetAccessToken <- function(access.token) {
        AccessToken(access.token)
    }
    GetAccessToken <- function() {
        return(access_token)
    }
    SetStartDate <- function(start.date) {
        StartDate(start.date)
    }
    GetStartDate <- function() {
        return(start.date)
    }
    SetEndDate <- function(end.date) {
        EndDate(end.date)
    }
    GetEndDate <- function() {
        return(end.date)
    }
    SetStartIndex <- function(start.index) {
        StartIndex(start.index)
    }
    GetStartIndex <- function() {
        return(start.index)
    }
    LoadAccessToken <- function() {
        load(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))
        return(token.list$access_token)
    }
    return(list(dimensions = Dimensions, metrics = Metrics, sort = Sort, 
        segments = Segment, filters = Filters, max.results = MaxResults, 
        table.id = TableID, to.uri = ToUri, clear.data = ClearData, 
        validate = Validate, access_token = AccessToken, Init = Init, 
        authorize = Authorize, SetAccessToken = SetAccessToken, 
        GetAccessToken = GetAccessToken, GetStartDate = GetStartDate, 
        GetEndDate = GetEndDate, GetAccessToken = GetAccessToken, 
        SetAccessToken = SetAccessToken, SetEndDate = SetEndDate, 
        SetStartDate = SetStartDate, SetStartIndex = SetStartIndex))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
