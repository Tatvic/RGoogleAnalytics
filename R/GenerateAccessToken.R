#' Gets an OAuth 2.0 Access Token by authorizing the user account to the Google 
#' Analytics API 
#' 
#' @details
#' When evaluated for the first time this function asks for User Consent
#' for the Google Analytics Account and retrieves the Access and Refresh Tokens
#' for Authorization. These tokens are saved locally to a file on the user's system.
#' If the user had authorized an account earlier and refresh token is already found
#' on the user's system, then this function retrives a new Access Token and updates
#' the Access Token File in user's memory.
#'
#' @export  
#' 
#' @param secrets.file.path Optional.
#' 
#' @importFrom rjson fromJSON
#' @importFrom RCurl postForm
GenerateAccessToken <- function(secrets.file.path = NULL) {
  
  # Build file path if path is provided by user

  if (is.null(secrets.file.path)) {
    internal.file.path <<- file.path(path.package("RGoogleAnalytics"), "client_secrets.json")
  } else {
    internal.file.path <<- secrets.file.path
  }
  

  if(!file.exists(internal.file.path)) {
    
    stop(message("client_secrets.json file does not exist at",internal.file.path,".Please specify a valid path"))
    
  } else {
    
    # API Credentials file exists  
    # Load the client_secrets file
    # client.secrets.json.file <- 
    client.secrets.list <- fromJSON(readLines(internal.file.path,warn = FALSE))
    
    # Build the URL string
    client.id <- as.character(client.secrets.list$installed$client_id) 
    client.secret <- as.character(client.secrets.list$installed$client_secret)
    redirect.uri <- as.character(client.secrets.list$installed$redirect_uris[1])
    
    # Check whether Access Token File is already present
    # If it isn't then redirect user to the Consent Screen and get new Access Token
    # If it is get a new Access Token based on the Refresh Token
    
    if(!file.exists(file.path(path.package("RGoogleAnalytics"), "accesstoken.rda"))) {
      
      url <- paste0('https://accounts.google.com/o/oauth2/auth?',
                    'scope=https://www.googleapis.com/auth/analytics.readonly&',          
                    'state=%2Fprofile&',
                    'redirect_uri=', redirect.uri, '&',
                    'response_type=code&',
                    'client_id=', client.id, '&',
                    'approval_prompt=force&',
                    'access_type=offline' 
      )
      
      # Get Auth Code
      # Load the prepared URL into a WWW browser.
      browseURL(url) 

      # Update this text
      cat("The Google Analytics data extraction process requires an authorization code.",
          "To accept the authorization code, you need to follow certain steps in your ",
          "browser. This code will help this R packge to generate the access",
          "token. Make sure you have already supplied credentials for installed app.",
          "\n\nSteps to be followed : \n1. Authorize your",
          "Google Analytics account by providing email and password. \n ",
          "\n2. Copy the generated code.")
      
      code <- readline(as.character(cat("\n\nPaste the authorization code here",
                                        ":=>")))
      
      
      cat("Retrieving the Access and Refresh Tokens based on the Authorization Code\n")
      
      # For retrieving the access token.
      token.list <- fromJSON(postForm('https://accounts.google.com/o/oauth2/token',
                                      code = code,
                                      client_id = client.id, 
                                      client_secret = client.secret,
                                      redirect_uri = redirect.uri,
                                      grant_type = "authorization_code",
                                      style = "POST"))
      
      # For saving the generated access token (as List data type with 
      # values - access_token, token_type, expires_in and refresh_token)
      # in file system where RGoogleAnalytics package located.
      
      # token.list contains the response by the Google Analytics API
      # Contents : access_token, token_type, refresh_token, expires_in
      # Retained the same naming convention as that followed by the Google Analytics API for 
      # these objects
      access.token <- token.list$access_token 
      
      save(token.list, 
           file = file.path(path.package("RGoogleAnalytics"),
                            "accesstoken.rda"))
      
      access.token.file.path <- as.character(file.path(path.package("RGoogleAnalytics"),
                                                       "accesstoken.rda"))                 
      
      cat("Access token has been saved to",access.token.file.path,"\n")
      
      return(invisible())
    } else {
      
      load(file.path(path.package("RGoogleAnalytics"),
                     "accesstoken.rda"))
      
      
      # Get new Access Token
      access.token <- RefreshToAccessToken(token.list$refresh_token, client.id,client.secret)
      
      #In case if a New Access Token is generated update it in the file as well
      token.list$access_token <- access.token
      
      #Save the updated credentials into the file
      save(token.list, 
           file = file.path(path.package("RGoogleAnalytics"),
                            "accesstoken.rda"))
      
      cat("Access token has been regenerated\n")
      
      return(invisible())  
    }
  }
}