#' Deletes App Credentials(Client ID and Client Secret) from the system
#'
#' In case if the user creates a new project in the Google API Developer Console, then
#' a fresh set of OAuth2.0 credentials (Client ID and Client Secret) are provided
#' This requires deletion of old stored credentials
#' 
#' 
#' 
#' @examples
#' \dontrun{RemoveAppCredentials()}  
#' 

RemoveAppCredentials <- function() {
  
  if(file.exists(file.path(path.package("RGoogleAnalytics"), "app_credentials.rda"))) {
    unlink(file.path(path.package("RGoogleAnalytics"),
                     "app_credentials.rda"),
           recursive = FALSE)
    cat("The Application Credentials have been deleted from your system\n")  
  } else {
    stop(cat("The Application Credentials file could not be found on your system\n"))
  }
  
}