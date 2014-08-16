#' This function takes the Refresh Token as an argument and retrives a new 
#' Access Token based on it
#' 
#' @keywords internal
#' Reference : https://developers.google.com/accounts/docs/OAuth2#installed
#' @param token.object OAuth2.0 Token Object containing refresh token, client ID and client Secret  
#'   
#' @return token.object Returns the same token object with the updated access token. Since the Token
#' object is a Reference Class object it can be modified in place
#'   
#' 
RefreshToAccessToken <- function(token.object){
  
  token.object <- token.object$refresh()
  
  return(token.object)
}