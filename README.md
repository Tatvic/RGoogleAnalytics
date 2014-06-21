
This repo is the new home for the RGoogleAnalytics library migrated from [Google Code SVN](https://code.google.com/p/r-google-analytics/)

## What is it

RGoogleAnalytics is a R Wrapper around the [Google Analytics](http://www.google.com/analytics/) API. It allows fast and easy data extraction in R so that further statistical analysis can be run on the data

## Key Features

* Provides Access to v3 of the [Google Analytics Core Reporting API](https://developers.google.com/analytics/devguides/reporting/core/v3/)

* Supports authorization via OAuth 2.0

* QueryBuilder Class to simplify creation of API Queries
 
* Ability to pull more than 10,000 rows of data in batches via pagination of queries

* Ability to mitigate the effect of Query Sampling by decreasing the date-range of queries and hence extract (nearly) unsampled data
 
* In cases where queries are sampled, the output also returns the percentage of sessions that were used for the query

## Installation

To get the current released version from CRAN:

```R
install.packages("RGoogleAnalytics")
```

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("Tatvic/RGoogleAnalytics")
```

## Dependencies

* [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) handles all the API Requests and Responses

* [rjson](http://cran.r-project.org/web/packages/rjson/index.html) is used to convert the JSON Responses delivered by the Google Analytics API into R Objects

* [lubridate](http://cran.r-project.org/web/packages/lubridate/index.html) handles the date manipulation logic underlying Query Partitioning

 

## Background

Work on RGoogleAnalytics was started by Michael Pearmain at Google. He was supported by Nick Mihailowski (Google) and Vignesh Prajapati(Tatvic). 

## Important Links

* [List](https://developers.google.com/analytics/devguides/reporting/core/dimsmets) of Valid Dimension/Metric Combinations from the Google Analytics API Reference Guide

* [Query Feed Explorer](http://ga-dev-tools.appspot.com/explorer/) allows you to test your queries for syntatical correctness. Once verified, the query parameters can then be copied to your R Script
