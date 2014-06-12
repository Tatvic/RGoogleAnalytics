  #' Save App Credentials (Client ID and Client Secret) locally to system
  #' 
  #' 
  #' This function gets the Client ID and Client Secret of the Application from
  #' the user and saves it locally to a file for OAuth 2.0 Authorization flow. 
  #'  
  #'   
  #' @param client.id Client ID of the Application 
  #' @param client.secret Client Secret of the Application
  #' 
  #' 
  #' @return Saves the App Credentials to a file on the user's system
  #' @author Kushan Shah
  
  GetAppCredentials <- function(client.id,
                                client.secret) {
     
    
    
    if(file.exists(file.path(system.file(package = "RGoogleAnalytics"),
                             "app_credentials.rda"))) {
      stop(cat("Your Application Credentials are already saved to your system. 
               Please use the RemoveAppCredentials Function to delete the credentials\n"))
    }
    
    # Argument Validation
    if (missing(client.id)) {
      stop(cat("Please specify a Client ID in the function arguments"))
    }
    
    if (missing(client.secret)) {
      stop(cat("Please specify a Client Secret in the function arguments"))
    }
    client.id <- as.character(client.id)
    client.secret <- as.character(client.secret)
    
    save(client.id,
         client.secret,
         file = file.path(system.file(package = "RGoogleAnalytics"),
                          "app_credentials.rda"))
    file.path <- as.character(file.path(system.file(package = "RGoogleAnalytics"),
                                        "app_credentials.rda"))
    cat("Your App Credentials have been saved to", file.path, "\n")
    }