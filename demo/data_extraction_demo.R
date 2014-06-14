# Ensure you have copied the file client_secrets.json to the correct location
# Authorize the Google Analytics account
Auth()

# Build a list of all the Query Parameters
query.list <- Init(start.date = "2013-11-28",
                   end.date = "2013-12-05",
                   dimensions = "ga:date,ga:pagePath,ga:hour",
                   metrics = "ga:sessions,ga:pageviews",
                   sort = "-ga:sessions",
                   max.results = 1234,
                   table.id = "ga:33093633")

# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query) 
