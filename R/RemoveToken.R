  #' Deletes the stored Access and Refresh Tokens from the local file 
  #'
  #' In case if the user wants to query a new profile, then the Authorization flow 
  #' has to repeated. This requires deleting the stored Access and Refresh Tokens
  #' from the system file
  #' 
  #' @export
  #' 
  #' @examples
  #' \dontrun{RemoveToken()}
  #' 
  #' 
  #' @author Vignesh Prajapati 
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