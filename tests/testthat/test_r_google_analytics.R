AccountFeedJSONString <- function() {
  	return('{\"kind\":\"analytics#profiles\",\"username\":\"vignesh@tatvic.com\",\"totalResults\":1,\"startIndex\":1,\"itemsPerPage\":1000,\"items\":[{\"id\":\"10696290\",\"kind\":\"analytics#profile\",\"selfLink\":\"https://www.googleapis.com/analytics/v3/management/accounts/5306665/webproperties/UA-5306665-1/profiles/10696290\",\"accountId\":\"5306665\",\"webPropertyId\":\"UA-5306665-1\",\"internalWebPropertyId\":\"10240597\",\"name\":\"tatvic.com\",\"currency\":\"USD\",\"timezone\":\"Asia/Calcutta\",\"websiteUrl\":\"http://www.tatvic.com/\",\"excludeQueryParameters\":\"fb_xd_fragment,tim,m,token,n\",\"siteSearchQueryParameters\":\"s\",\"type\":\"WEB\",\"created\":\"2008-08-15T14:45:21.000Z\",\"updated\":\"2012-12-10T13:47:52.046Z\",\"eCommerceTracking\":false,\"parentLink\":{\"type\":\"analytics#webproperty\",\"href\":\"https://www.googleapis.com/analytics/v3/management/accounts/5306665/webproperties/UA-5306665-1\"},\"childLink\":{\"type\":\"analytics#goals\",\"href\":\"https://www.googleapis.com/analytics/v3/management/accounts/5306665/webproperties/UA-5306665-1/profiles/10696290/goals\"}}]}')
}



#This test requires rjson loaded
#How to handle?

test_that("Account Feed is parsed properly", {
	ProfileName <- c("tatvic.com")
	TableId <- c("10696290")
  	test.profile <- data.frame(ProfileName = ProfileName, TableId = TableId)
  	data <- GetProfilesFromJSON(AccountFeedJSONString())
  	profile.data <- data.frame(ProfileName = data$profiles$name,
                             TableId = data$profiles$id)
  	expect_that(test.profile,equals(profile.data))
  	expect_that(1,equals(as.numeric(data$totalResults)))
})

SampleErrorJSONString <- function() {
  	return('{\"error\":{\"errors\":[{\"domain\":\"global\",\"reason\":\"badRequest\",\"message\":\"Sort key ga:foo is not a dimension or metric in this query.\"}],\"code\":400,\"message\":\"Sort key ga:foo is not a dimension or metric in this query.\"}}')
}

test_that("Error response from GA API is parsed", {
	error.message <- ParseApiErrorMessage(SampleErrorJSONString())
  	expect_that(400, equals(error.message$code))
  	expect_that('Sort key ga:foo is not a dimension or metric in this query.',
  		equals(error.message$message))
})

DataFeedJSONString <- function(){
  	return('{\"kind\":\"analytics#gaData\",\"id\":\"https://www.googleapis.com/analytics/v3/data/ga?ids=ga:10696290&dimensions=ga:source,ga:medium&metrics=ga:sessions,ga:bounces&start-date=2013-01-03&end-date=2013-01-03&start-index=1&max-results=4\",\"query\":{\"start-date\":\"2013-01-03\",\"end-date\":\"2013-01-03\",\"ids\":\"ga:10696290\",\"dimensions\":\"ga:source,ga:medium\",\"metrics\":[\"ga:sessions\",\"ga:bounces\"],\"start-index\":1,\"max-results\":4},\"itemsPerPage\":4,\"totalResults\":28,\"selfLink\":\"https://www.googleapis.com/analytics/v3/data/ga?ids=ga:10696290&dimensions=ga:source,ga:medium&metrics=ga:sessions,ga:bounces&start-date=2013-01-03&end-date=2013-01-03&start-index=1&max-results=4\",\"nextLink\":\"https://www.googleapis.com/analytics/v3/data/ga?ids=ga:10696290&dimensions=ga:source,ga:medium&metrics=ga:sessions,ga:bounces&start-date=2013-01-03&end-date=2013-01-03&start-index=5&max-results=4\",\"profileInfo\":{\"profileId\":\"10696290\",\"accountId\":\"5306665\",\"webPropertyId\":\"UA-5306665-1\",\"internalWebPropertyId\":\"10240597\",\"profileName\":\"tatvic.com\",\"tableId\":\"ga:10696290\"},\"containsSampledData\":false,\"columnHeaders\":[{\"name\":\"ga:source\",\"columnType\":\"DIMENSION\",\"dataType\":\"STRING\"},{\"name\":\"ga:medium\",\"columnType\":\"DIMENSION\",\"dataType\":\"STRING\"},{\"name\":\"ga:sessions\",\"columnType\":\"METRIC\",\"dataType\":\"INTEGER\"},{\"name\":\"ga:bounces\",\"columnType\":\"METRIC\",\"dataType\":\"INTEGER\"}],\"totalsForAllResults\":{\"ga:sessions\":\"148\",\"ga:bounces\":\"94\"},\"rows\":[[\"(direct)\",\"(none)\",\"27\",\"17\"],[\"Tatvic Newsletter\",\"email\",\"1\",\"1\"],[\"analytics.blogspot.com\",\"referral\",\"2\",\"0\"],[\"analytics.blogspot.in\",\"referral\",\"1\",\"0\"]]}')
}

# test_that("GA API DataFeed is parsed properly", {
# 	DataFeed.json <- ga$ParseDataFeedJSON(DataFeedJSONString())
# 	DataFeed.json.totalResults <- DataFeed.json$totalResults
# 	DataFeed.json.totalsForAllResults.sessions <- 
# 		as.numeric(DataFeed.json$totalsForAllResults$`ga.sessions`)
# 	DataFeed.json.totalsForAllResults.bounces <- 
# 		as.numeric(DataFeed.json$totalsForAllResults$`ga.bounces`)

# 	# Testing the $total.results is as expected
# 	test.total.results <- 28
#  	expect_that(test.total.results, equals(as.numeric(DataFeed.json.totalResults)))

# 	# ga.source  <- c("(direct)",
#  #                  "Tatvic Newsletter",
#  #                  "analytics.blogspot.com",
#  #                  "analytics.blogspot.in")
#  #  	ga.medium  <- c("(none)","email","referral","referral")
#  #  	ga.sessions  <- c(27,1,2,1)
#  #  	ga.bounces <- c(17,1,0,0)
  
#  #  	test.data  <- data.frame(ga.source,
#  #                           ga.medium,
#  #                           ga.sessions, 
#  #                           ga.bounces,
#  #                           stringsAsFactors = FALSE)
  
#  #  	names(test.data) <- c("source", "medium", "sessions", "bounces")

#   	#expect_that(test.data,equals(DataFeed.json))

#  	# Testing aggr.totals match
#   	ga.sessions <- 148
#   	ga.bounces <- 94
  
#   	test.aggr.totals <- as.data.frame(cbind(ga.sessions, ga.bounces))
#   	#names(test.aggr.totals) <- "aggregate.totals"
#   	#rownames(test.aggr.totals) <- c("ga:sessions", "ga:bounces")
  
#   	json.aggr.totals <- 
#     	as.data.frame(cbind(as.numeric(DataFeed.json.totalsForAllResults.sessions), 
#         	                as.numeric(DataFeed.json.totalsForAllResults.bounces)))
#   	names(json.aggr.totals) <- c("ga.sessions","ga.bounces")
#   	#rownames(json.aggr.totals) <- c("ga:sessions", "ga:bounces")
  
#   	expect_that(test.aggr.totals,equals(json.aggr.totals))
# })
