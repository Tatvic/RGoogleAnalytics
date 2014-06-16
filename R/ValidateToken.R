#' Check whether the Access Token has expired
#' 
#' This function checks whether the Access Token stored in the local file is 
#' expired. If yes, it generates a new Access Token and updates the local file.
#' 
#'  
#' @export
#'  

ValidateToken <- function(token) {
  
  # Check whether there is a previously stored access token
  
  if (!token$validate()) {
    RefreshToAccessToken(token)
    cat("Access Token successfully updated\n")
  } else {
    cat("Access Token is valid\n")
  }
  
  return(invisible())   
}