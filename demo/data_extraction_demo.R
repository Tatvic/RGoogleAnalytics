
ValidateToken()
RemoveToken()
RemoveAppCredentials()

GetAppCredentials("150487456763-263oc9mh8q1427c788fl6piahlbcuqm3.apps.googleusercontent.com",
                     "TUmTHIdHFyHoeDhFad_TknUI")

GenerateAccessToken()

query_list <- Init(start.date = "2013-11-28",
                   end.date = "2013-12-05",
                   dimensions = "ga:date,ga:pagePath,ga:hour",
                   metrics = "ga:visits,ga:pageviews",
                   max.results = 10000,
                   table.id = "ga:33093633")

query <- QueryBuilder(query_list)

ga.data <- GetReportData(query)
