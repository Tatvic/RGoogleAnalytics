library(lubridate)
library(rjson)
library(RCurl)

query <- QueryBuilder()
ga <- RGoogleAnalytics()

ga$ValidateToken()
ga$RemoveToken()
ga$RemoveAppCredentials()

ga$GetAppCredentials("150487456763-263oc9mh8q1427c788fl6piahlbcuqm3.apps.googleusercontent.com",
                     "TUmTHIdHFyHoeDhFad_TknUI")

ga$GenerateAccessToken()

query$Init(start.date = "2013-11-28",
           end.date = "2013-12-05",
           dimensions = "ga:date,ga:pagePath,ga:hour",
           metrics = "ga:visits,ga:pageviews",
           max.results = 10000,
           table.id = "33093633")

ga.data <- ga$GetReportData(query,paginate_query=T)
