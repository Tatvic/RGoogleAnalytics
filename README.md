
This repo is the new home for the RGoogleAnalytics library migrated from Google Code SVN(https://code.google.com/p/r-google-analytics/)

Key Features

* Provides Access to v3 of the Google Analytics Core Reporting API

* Supports authorization via OAuth 2.0

* QueryBuilder Class to simplify creation of API Queries
 
* Ability to pull more than 10,000 rows of data in batches via pagination of queries

* Ability to mitigate the effect of Query Sampling by decreasing the date-range of queries and hence get (nearly) unsampled data
 
* In cases where queries are sampled, the output also returns the percentage of sessions that were used for the query

## Installation

To get the current released version from CRAN:

```R
install.packages("RGoogleAnalytics")
```

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("")
```