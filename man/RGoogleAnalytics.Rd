\name{RGoogleAnalytics}
\alias{RGoogleAnalytics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a skeleton shell for accessing the Google Analytics API.
}
\description{
This is the main class that provides methods to authenticate and extract data from the Google Analytics API
}
\usage{
ga <- RGoogleAnalytics()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
GetProfileData()       
  \item{GetReportData()}{Function to Extract Data from the Google Analytics API}       
  \item{GetAppCredentials()}{Function to save Client ID and Client Secret to  a local file }
  \item{RemoveToken()}{Function to delete OAuth 2.0 Access and Refresh Tokens from the local file}
  \item{RemoveAppCredentials()}{Function to delete Client ID and Client Secret from the local file}       
  \item{GenerateAccessToken()}{Function to authenticate Google Analytics Account via OAuth 2.0 and get Access Token}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael Pearmain,Nick Mihailowski,Vignesh Prajapati, Kushan Shah
}
\note{
%%  ~~further notes~~
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    kMaxDefaultRows <- 10000
    kMaxPages <- 100
    query.uri <- NULL
    dataframe.param <- data.frame()
    options(RCurlOptions = list(capath = system.file("CurlSSL", 
        "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
    GetAppCredentials <- function(client.id, client.secret) {
        if (file.exists(file.path(system.file(package = "RGoogleAnalytics"), 
            "app_credentials.rda"))) {
            stop(cat("Your Application Credentials are already saved to your system. \n               Please use the RemoveAppCredentials Function to delete the credentials\n"))
        }
        if (missing(client.id)) {
            stop(cat("Please specify a Client ID in the function arguments"))
        }
        if (missing(client.secret)) {
            stop(cat("Please specify a Client Secret in the function arguments"))
        }
        client.id <- as.character(client.id)
        client.secret <- as.character(client.secret)
        save(client.id, client.secret, file = file.path(system.file(package = "RGoogleAnalytics"), 
            "app_credentials.rda"))
        file.path <- as.character(file.path(system.file(package = "RGoogleAnalytics"), 
            "app_credentials.rda"))
        cat("Your App Credentials have been saved to", file.path, 
            "\n")
    }
    GenerateAccessToken <- function() {
        if (!file.exists(file.path(path.package("RGoogleAnalytics"), 
            "accesstoken.rda"))) {
            if (!file.exists(file.path(path.package("RGoogleAnalytics"), 
                "app_credentials.rda"))) {
                stop(cat("Application Credentials do not exist.Please use the GetAppCredentials \n                 function to save the credentials to a local file"))
            }
            else {
                load(file.path(path.package("RGoogleAnalytics"), 
                  "app_credentials.rda"))
                client.id <- as.character(client.id)
                client.secret <- as.character(client.secret)
                redirect.uri <- "urn:ietf:wg:oauth:2.0:oob"
                url <- paste0("https://accounts.google.com/o/oauth2/auth?", 
                  "scope=https://www.googleapis.com/auth/analytics.readonly&", 
                  "state=\%2Fprofile&", "redirect_uri=", redirect.uri, 
                  "&", "response_type=code&", "client_id=", client.id, 
                  "&", "approval_prompt=force&", "access_type=offline")
                browseURL(url)
                cat("The Google Analytics data extraction process requires an authorization code.", 
                  "To accept the authorization code, you need to follow certain steps in your ", 
                  "browser. This code will help this R packge to generate the access", 
                  "token. Make sure you have already supplied credentials for installed app.", 
                  "\n\nSteps to be followed : \n1. Authorize your", 
                  "Google Analytics account by providing email and password. \n ", 
                  "\n2. Copy the generated code.")
                code <- readline(as.character(cat("\n\nPaste the authorization code here", 
                  ":=>")))
                cat("Retrieving the Access and Refresh Tokens based on the Authorization Code\n")
                token.list <- fromJSON(postForm("https://accounts.google.com/o/oauth2/token", 
                  code = code, client_id = client.id, client_secret = client.secret, 
                  redirect_uri = redirect.uri, grant_type = "authorization_code", 
                  style = "POST"))
                access.token <- token.list$access_token
                save(token.list, file = file.path(path.package("RGoogleAnalytics"), 
                  "accesstoken.rda"))
                access.token.file.path <- as.character(file.path(path.package("RGoogleAnalytics"), 
                  "accesstoken.rda"))
                cat("Access token has been saved to", access.token.file.path, 
                  "\n")
                return(invisible())
            }
        }
        else {
            load(file.path(path.package("RGoogleAnalytics"), 
                "accesstoken.rda"))
            load(file.path(path.package("RGoogleAnalytics"), 
                "app_credentials.rda"))
            access.token <- RefreshToAccessToken(token.list$refresh_token, 
                client.id, client.secret)
            token.list$access_token <- access.token
            save(token.list, file = file.path(path.package("RGoogleAnalytics"), 
                "accesstoken.rda"))
            cat("Access token has been regenerated\n")
            return(invisible())
        }
    }
    RefreshToAccessToken <- function(refresh.token, client.id, 
        client.secret) {
        refresh.token.list = fromJSON(postForm("https://accounts.google.com/o/oauth2/token", 
            refresh_token = refresh.token, client_id = client.id, 
            client_secret = client.secret, grant_type = "refresh_token", 
            style = "POST"))
        return(refresh.token.list$access_token)
    }
    RemoveToken <- function() {
        if (file.exists(file.path(path.package("RGoogleAnalytics"), 
            "accesstoken.rda"))) {
            unlink(file.path(path.package("RGoogleAnalytics"), 
                "accesstoken.rda"), recursive = FALSE)
            cat("The Access token has been deleted from your system\n")
        }
        else {
            stop(cat("The Access token file could not be found on your system\n"))
        }
    }
    RemoveAppCredentials <- function() {
        if (file.exists(file.path(path.package("RGoogleAnalytics"), 
            "app_credentials.rda"))) {
            unlink(file.path(path.package("RGoogleAnalytics"), 
                "app_credentials.rda"), recursive = FALSE)
            cat("The Application Credentials have been deleted from your system\n")
        }
        else {
            stop(cat("The Application Credentials file could not be found on your system\n"))
        }
    }
    ValidateToken <- function() {
        load(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))
        api.response.json <- getURL(paste0("https://www.googleapis.com/oauth2/v1/", 
            "tokeninfo?access_token=", token.list$access_token))
        api.response.list <- fromJSON(api.response.json, method = "C")
        check.token.param <- regexpr("error", api.response.json)
        if (check.token.param[1] != -1) {
            cat("Access Token had expired. Regenerating access token\n")
            GenerateAccessToken()
            return(invisible())
        }
    }
    SetDataFrame <- function(GA.list.param.columnHeaders, dataframe.param) {
        column.param <- t(sapply(GA.list.param.columnHeaders, 
            "[", 1:max(sapply(GA.list.param.columnHeaders, length))))
        col.name <- gsub("ga:", "", as.character(column.param[, 
            1]))
        col.datatype <- as.character(column.param[, 3])
        colnames(dataframe.param) <- col.name
        dataframe.param <- as.data.frame(dataframe.param)
        dataframe.param <- SetColDataType(col.datatype, col.name, 
            dataframe.param)
        return(dataframe.param)
    }
    SetColDataType <- function(col.datatype, col.name, dataframe.param) {
        for (i in 1:length(col.datatype)) {
            if (col.datatype[i] == "STRING") {
                dataframe.param[, i] <- as.character(dataframe.param[, 
                  i])
            }
            else {
                dataframe.param[, i] <- as.numeric(as.character(dataframe.param[, 
                  i]))
            }
        }
        return(dataframe.param)
    }
    GetProfilesFromJSON <- function(api.response.json) {
        GA.profiles <- ParseApiErrorMessage(api.response.json)
        TotalProfiles <- GA.profiles$totalResults
        if (!is.null(GA.profiles$code)) {
            stop(paste("code: ", GA.profiles$code, "Reason: ", 
                GA.profiles$message))
        }
        GA.profiles.param <- t(sapply(GA.profiles$items, "[", 
            1:max(sapply(GA.profiles$items, length))))
        profiles.id <- as.character(GA.profiles.param[, 1])
        profiles.name <- as.character(GA.profiles.param[, 7])
        if (length(profiles.id) == 0) {
            stop("Please check the access token. It may be invalid or expired")
        }
        else {
            profiles <- data.frame(id = profiles.id, name = profiles.name, 
                stringsAsFactors = FALSE)
            profileres.list <- list(totalResults = TotalProfiles, 
                profiles = profiles)
            return(profileres.list)
        }
    }
    GetAcctDataFeedJSON <- function(query.uri) {
        GA.Data <- getURL(query.uri)
        return(GA.Data)
    }
    GetProfileData <- function() {
        ValidateToken()
        load(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))
        query.uri <- paste0("https://www.googleapis.com/analytics/v3/", 
            "management/accounts/~all/webproperties/~all/", "profiles?access_token=", 
            token.list$access_token)
        if (!is.character(query.uri)) {
            stop("The query.uri parameter must be a character string")
        }
        api.response.json <- GetAcctDataFeedJSON(query.uri)
        profiles.param <- GetProfilesFromJSON(api.response.json)
        return(profiles.param$profiles)
    }
    ParseDataFeedJSON <- function(GA.Data) {
        GA.list.param <- ParseApiErrorMessage(GA.Data)
        if (!is.null(GA.list.param$code)) {
            stop(paste("code :", GA.list.param$code, "Reason :", 
                GA.list.param$message))
        }
        return(GA.list.param)
    }
    GetDataFeed <- function(query.uri) {
        GA.Data <- getURL(query.uri)
        GA.list <- ParseDataFeedJSON(GA.Data)
        if (is.null(GA.list$rows)) {
            cat("Your query matched 0 results. Please verify your query.")
            break
        }
        else {
            return(GA.list)
        }
    }
    GetReportData <- function(query.builder, split_daywise = FALSE, 
        paginate_query = FALSE) {
        query.builder$validate()
        kMaxDefaultRows <- 10000
        max.rows <- query.builder$max.results()
        if (split_daywise != T && paginate_query != T) {
            ga.list <- GetDataFeed(query.builder$to.uri())
            total.results <- ga.list$totalResults
            items.per.page <- ga.list$itemsPerPage
            contains.sampled.data <- ga.list$containsSampledData
            response.size <- length(ga.list$rows)
            if (total.results < kMaxDefaultRows) {
                max.rows <- kMaxDefaultRows
            }
            if (length(query.builder$dimensions()) == 0) {
                totalrows <- 1
                dataframe.param <- ga.list$rows[[1]]
                dim(dataframe.param) <- c(1, length(dataframe.param))
            }
            else {
                totalrows <- nrow(do.call(rbind, as.list(ga.list$rows)))
                dataframe.param <- rbind(dataframe.param, do.call(rbind, 
                  as.list(ga.list$rows)))
            }
            final.df <- SetDataFrame(ga.list$columnHeaders, dataframe.param)
            if (length(ga.list$rows) < total.results) {
                cat("Status of Query:\n")
                cat("The API returned", response.size, "results out of", 
                  total.results, "results\n")
                cat("In order to get all results, set paginate_query = T in the GetReportData function.\n")
                if (max.rows < kMaxDefaultRows) {
                  cat("Set max.rows = 10000 for efficient query utilization while Paginating\n")
                }
            }
            else {
                cat("Status of Query:\n")
                cat("The API returned", response.size, "results")
            }
            if (contains.sampled.data == T) {
                visits.for.sampled.query <- round(100 * (as.integer(ga.list$sampleSize)/as.integer(ga.list$sampleSpace)), 
                  2)
                cat("The query response contains sampled data. It is based on ", 
                  visits.for.sampled.query, "\% of your visits.\n")
                cat("You can split the query day-wise in order to reduce the effect of sampling.\n")
                cat("Set split_daywise = T in the GetReportData function\n")
                cat("Note that split_daywise = T will automatically invoke Pagination in each sub-query\n")
            }
        }
        else if (split_daywise == T) {
            GA.DF <- SplitQueryDaywise(query.builder)
            final.df <- SetDataFrame(GA.DF$header, GA.DF$data)
            cat("The API returned", nrow(final.df), "results\n")
        }
        else if (paginate_query == T) {
            ga.list <- GetDataFeed(query.builder$to.uri())
            ga.list.df <- data.frame()
            ga.list.df <- rbind(ga.list.df, do.call(rbind, as.list(ga.list$rows)))
            if (length(ga.list$rows) < ga.list$totalResults) {
                number.of.pages <- ceiling(ga.list$totalResults/length(ga.list$rows))
                if (number.of.pages > 100) {
                  number.of.pages <- kMaxPages
                }
                paged.query.list <- PaginateQuery(query.builder, 
                  number.of.pages)
                inter.df <- rbind(ga.list.df, paged.query.list$data)
                final.df <- SetDataFrame(paged.query.list$headers, 
                  inter.df)
                cat("The API returned", nrow(final.df), "results\n")
            }
            else {
                stop("Pagination is not required in the query.Set Paginate_Query = F and re-run the query\n")
            }
        }
        return(final.df)
    }
    SplitQueryDaywise <- function(query.builder) {
        ValidateToken()
        load(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))
        query.builder$SetAccessToken(token.list$access_token)
        start.date <- ymd(query.builder$GetStartDate())
        end.date <- ymd(query.builder$GetEndDate())
        date.difference <- as.numeric(difftime(end.date, start.date, 
            units = "days"))
        if (date.difference == 0) {
            stop("Please verify start date and end date. They cannot be the same")
        }
        master.df <- data.frame()
        for (i in (0:date.difference)) {
            date <- format(as.POSIXct(start.date) + days(i), 
                "\%Y-\%m-\%d")
            cat("Run", i, "of", date.difference, "Getting data for", 
                date, "\n")
            query.builder$SetStartDate(date)
            query.builder$SetEndDate(date)
            query.builder$SetStartIndex(as.numeric(1))
            first.query.df <- data.frame()
            first.query <- GetDataFeed(query$to.uri())
            first.query.df <- rbind(first.query.df, do.call(rbind, 
                as.list(first.query$rows)))
            if (length(first.query$rows) < first.query$totalResults) {
                number.of.pages <- ceiling((first.query$totalResults)/length(first.query$rows))
                if (number.of.pages > 100) {
                  number.of.pages <- kMaxPages
                }
                inter.df <- PaginateQuery(query.builder, number.of.pages)
                inter.df <- rbind(first.query.df, inter.df$data)
                master.df <- rbind(master.df, inter.df)
            }
            else {
                master.df <- rbind(first.query.df, master.df)
            }
        }
        master.df <- rbind(first.query.df, master.df)
        return(list(header = first.query$columnHeaders, data = master.df))
    }
    PaginateQuery <- function(query.builder, pages) {
        ValidateToken()
        load(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))
        query.builder$SetAccessToken(token.list$access_token)
        df.inner <- data.frame()
        for (i in (1:(pages - 1))) {
            dataframe.param <- data.frame()
            start.index <- (i * kMaxDefaultRows) + 1
            cat("Getting data starting at row", start.index, 
                "\n")
            query.builder$SetStartIndex(start.index)
            ga.list <- GetDataFeed(query.builder$to.uri())
            dataframe.param <- rbind(dataframe.param, do.call(rbind, 
                as.list(ga.list$rows)))
            df.inner <- rbind(df.inner, dataframe.param)
            col.headers <- ga.list$columnHeaders
            rm(ga.list)
        }
        return(list(headers = col.headers, data = df.inner))
    }
    ParseApiErrorMessage <- function(api.response.json) {
        api.response.list <- fromJSON(api.response.json, method = "C")
        check.param <- regexpr("error", api.response.list)
        if (check.param[1] != -1) {
            return(list(code = api.response.list$error$code, 
                message = api.response.list$error$message))
        }
        else {
            code <- NULL
            return(api.response.list)
        }
    }
    return(list(GetProfileData = GetProfileData, GetReportData = GetReportData, 
        GetAppCredentials = GetAppCredentials, GetProfilesFromJSON = GetProfilesFromJSON, 
        ParseApiErrorMessage = ParseApiErrorMessage, ParseDataFeedJSON = ParseDataFeedJSON, 
        GenerateAccessToken = GenerateAccessToken, RefreshToAccessToken = RefreshToAccessToken, 
        RemoveToken = RemoveToken, ValidateToken = ValidateToken, 
        RemoveAppCredentials = RemoveAppCredentials, kMaxDefaultRows = kMaxDefaultRows))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
