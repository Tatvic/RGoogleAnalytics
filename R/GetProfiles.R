#' Retrieves the list of Profiles for the Google Analytics Account
#' 
#' This function retrives all the available profiles from the 
#' Google analytics account which has been authenticated by the user 
#'  
#' @export 
#' @examples \dontrun{GetProfiles()}
#' @return profiles R dataframe with profile id and profile name.

GetProfiles <- function() {
  
  ValidateToken()
  
  load(file.path(path.package("RGoogleAnalytics"),
                 "accesstoken.rda"))

  query.uri <- paste0('https://www.googleapis.com/analytics/v3/',
                      'management/accounts/~all/webproperties/~all/',
                      'profiles?access_token=',
                      token.list$access_token
  )
  if (!is.character(query.uri)) {
    stop("The query.uri parameter must be a character string")
  }
  
  # This api.reponse should be in json format
  api.response.json <- GetAcctDataFeedJSON(query.uri)
  profiles.param <- GetProfilesFromJSON(api.response.json)
  return(profiles.param$profiles)
}