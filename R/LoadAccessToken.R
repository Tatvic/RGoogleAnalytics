#' Loads the access token from the system memory into R

LoadAccessToken <- function() {
  load(file.path(path.package("RGoogleAnalytics"),"accesstoken.rda"))
  return(token.list$access_token)
}