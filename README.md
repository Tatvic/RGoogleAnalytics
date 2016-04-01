Branch with the goal to integrate the [Multi Channel Funnel API](https://developers.google.com/analytics/devguides/reporting/mcf/v3/) into RGoogleAnalytics.

## Idea

* Load additional data from MCF API.
* Yet only Google Analytics Standard Reporting API supported.
* Use authentication and query builder from RGoogleAnalytics for MCF API.

## Approach so far

```R
# Create a list of Query Parameters
query.list <- Init(start.date = "2014-11-01",
                   end.date = "2014-11-02",
                   dimensions = "mcf:sourcePath",
                   metrics = "mcf:totalConversions,mcf:totalConversionValue",
                   sort = "-mcf:totalConversions",
                   max.results = 1000,
                   table.id = "ga:*********")

# Create the query object
ga.query <- QueryBuilder(query.list)

# Fire the query to the Google Analytics API
ga.df <- GetReportDataMCF(ga.query, oauth_token)
```

## To DOs and Shortcomings

* `r GetReportDataMCF()` returns dataframe which contains data in lists
* Parse data into dataframe, eliminate lists  
* If argument `r split_daywise` or `r paginate_query` set to TRUE, `r GetReportDataMCF()` returns an error