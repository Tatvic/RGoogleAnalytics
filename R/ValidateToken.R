#' Check whether the Access Token has expired
#' 
#' This function checks whether the Access Token is 
#' expired. If yes, it generates a new Access Token and updates the token object.
#' 
#' @param token Token object containing the OAuth authentication parameters
#'    
#' @export
#'  

ValidateToken <- function(token) {
  
  # Check whether there is a previously stored access token
  
  if (!token$validate()) {
    RefreshToAccessToken(token)
    message("Access Token successfully updated.")
  } else {
    message("Access Token is valid.")
  }
  
  return(invisible())   
}