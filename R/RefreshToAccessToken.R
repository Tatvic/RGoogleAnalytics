#' This function takes the Refresh Token as an argument and retrives a new 
#' Access Token based on it
#' 
#' @keywords internal
#' Reference : https://developers.google.com/accounts/docs/OAuth2#installed
#' @param refresh.token  Refresh Token that was saved to the local file
#' @param client.id      Client ID of the Application. This is a OAuth2.0 Credential   
#' @param client.secret  Client Secret of the Application. Again this too is an
#'                   OAuth2.0 Credential   
#'   
#' @return access token   New Access Token
#'   
#' 
RefreshToAccessToken <- function(token.object){
  
  token.object <- token.object$refresh()
  
  return(token.object)
}