
This repo is the new home for the RGoogleAnalytics library migrated from [Google Code SVN](https://code.google.com/p/r-google-analytics/)

## What is it

RGoogleAnalytics is a R Wrapper around the [Google Analytics](http://www.google.com/analytics/) API. It allows fast and easy data extraction in R so that further statistical analysis can be run on the data

## Key Features

* Provides Access to v3 of the [Google Analytics Core Reporting API](https://developers.google.com/analytics/devguides/reporting/core/v3/)

* Ability to **pull more than 10,000 rows of data** in batches via **pagination** of queries

* Ability to **mitigate the effect of Query Sampling** by splitting the date-range of queries and hence extract (nearly) unsampled data

* Ability to **cache data** fetched from Google

* Supports authorization via OAuth 2.0
  
* In cases where queries are sampled, the output also returns the percentage of sessions that were used for the query

## Installation


To get the current development version from github:

```R
# require(devtools)
devtools::install_github("Tatvic/RGoogleAnalytics")
```

## Dependencies

* [httr](http://cran.r-project.org/web/packages/httr/index.html) handles the underlying OAuth2.0 Authorization flow and the API requests

* [lubridate](http://cran.r-project.org/web/packages/lubridate/index.html) handles the date manipulation logic underlying Query Partitioning

## Background

Work on RGoogleAnalytics was started by Michael Pearmain at Google. He was supported by Nick Mihailowski (Google) and Vignesh Prajapati (Tatvic). 

## Tutorials and Use-cases

* [Basic](https://github.com/LucyMcGowan/Tutorials/blob/master/googleanalytics.Rmd) tutorial to get started linking Google Analytics to an API pull
* Under development

## Important Links

* [List](https://developers.google.com/analytics/devguides/reporting/core/dimsmets) of Valid Dimension/Metric Combinations from the Google Analytics API Reference Guide

* [Query Feed Explorer](http://ga-dev-tools.appspot.com/explorer/) allows you to test your queries for syntatical correctness. Once verified, the query parameters can then be copied to your R Script

* [Demo](http://www.tatvic.com/blog/google-analytics-data-extraction-in-r/) link on how to use this package to extract data from Google Analytics.
