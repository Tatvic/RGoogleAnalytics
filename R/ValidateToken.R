  #' Check whether the Access Token has expired
  #' 
  #' This function checks whether the Access Token stored in the local file is 
  #' expired. If yes, it generates a new Access Token and updates the local file.
  #' 
  #'  
  #' @export
  #'  

  ValidateToken <- function() {
    
    # Check whether there is a previously stored access token
    
    if(!file.exists(file.path(path.package("RGoogleAnalytics"),
                              "accesstoken.rda"))) {
      stop("No Access Token found on system. Please re-run the Auth() function")
    } 
      
      load(file.path(path.package("RGoogleAnalytics"),"accesstoken.rda"))
                       
    
    
    api.response.json <- getURL(paste0("https://www.googleapis.com/oauth2/v1/",
                                       "tokeninfo?access_token=",
                                       token.list$access_token
    ))
    api.response.list <- fromJSON(api.response.json, method = 'C')  
    check.token.param <- regexpr("error", api.response.json)
    
    if (check.token.param[1] != -1) {
      # If token has expired Generate a New Access token
      cat("Access Token had expired. Regenerating access token\n") 

      if(!file.exists(file.path(path.package("RGoogleAnalytics"),"client_secrets.json"))) {
        stop("client_secrets.json is missing at location ",file.path(path.package("RGoogleAnalytics")))
      } 
      
      # Load the file client_secrets.json
      client.secrets.list <- fromJSON(readLines(file.path(path.package("RGoogleAnalytics"),"client_secrets.json"),warn = FALSE))
      client.id <- as.character(client.secrets.list$installed$client_id) 
      client.secret <- as.character(client.secrets.list$installed$client_secret)
      
      new.access.token <- RefreshToAccessToken(token.list$refresh_token,client.id,client.secret)

      token.list$access_token <- new.access.token

      #Save the updated credentials into the file
      save(token.list, 
           file = file.path(path.package("RGoogleAnalytics"),
                            "accesstoken.rda"))

      cat("Access Token successfully updated")
      
      return(invisible())
    } else {

      cat("Current Access Token is valid\n")
      
      return(invisible())
    }   
  }