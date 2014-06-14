  #' Deletes the stored Access and Refresh Tokens from the local file 
  #'
  #' The OAuth 2.0 credentials are account specific. If the user wants to query 
  #' a different Google Analytics Account than the one previously authenticated
  #' the older credentials need to be deleted from the system. This function first 
  #' searches for the credentials file in the system and deletes the file if found
  #' 
  #' 
  #' @export
  #' 
  #' @examples
  #' \dontrun{RemoveToken()}
  #' 
  RemoveToken <- function() {
        
    if(file.exists(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))) {
      unlink(file.path(path.package("RGoogleAnalytics"),
                       "accesstoken.rda"),
             recursive = FALSE)
      cat("The Access token has been deleted from your system\n")    
    } else {
      stop(cat("The Access token file could not be found on your system\n"))
    }
    
  }