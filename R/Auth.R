#' Authorizes the RGoogleAnalytics library to the user's Google Analytics Account 
#' 
#' @description
#' This function expects a client_secrets.json file. In order to obtain this file, you will
#' have to Register an app with the Google Analytics API
#' 
#' - Go to \url{https://console.developers.google.com} 
#' 
#' - Create a New Project and enable the Google Analytics API
#' 
#' - On the Credentials screen, create a new Client ID for Application Type "Installed Application". 
#' 
#' - Download the JSON file and rename it to client_secrets.json
#' 
#' - Copy it to the RGoogleAnalytics installation directory.
#' It can be found by running \code{file.path(path.package("RGoogleAnalytics"))}
#' 
#' 
#' @details
#' When evaluated for the first time this function asks for User Consent
#' for the Google Analytics Account and retrieves the Access and Refresh Tokens
#' for Authorization. These tokens are saved locally to a file on the user's system.
#' In subsequent executions, a browser redirect is not required. This function will automatically
#' get a New Access Token each time when called. Note that an Access Token is valid for 60 minutes
#'
#' @export  
#' 
#' 
#' @importFrom rjson fromJSON
#' @importFrom RCurl postForm
Auth <- function() {
  
  # Update this function in future so that user can specify a custom file path

  internal.file.path <- file.path(path.package("RGoogleAnalytics"), "client_secrets.json")

  if(!file.exists(internal.file.path)) {
    
    stop(message("client_secrets.json file does not exist at ",
                 file.path(path.package("RGoogleAnalytics"))))
    
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
          "browser. This code will help this R package to generate the access",
          "token. Make sure you have already supplied credentials for installed app.",
          "\n\nSteps to be followed : \n1. Authorize your",
          "Google Analytics account by providing email and password. \n ",
          "\n2. Copy the generated code.")
      
      code <- readline(as.character(cat("\n\nPaste the authorization code here",
                                        ":=>")))
      
      
      cat("Retrieving the Access and Refresh Tokens based on the Authorization Code\n")
      
      options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
      
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
      
      cat("Tokens have been saved to",access.token.file.path,"\n")
      
      return(invisible())
    } else {
      
      cat("Found Access Token on system\n")
      cat("Updating Access Token\n")

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
      
      cat("Access token successfully updated\n")
      
      return(invisible())  
    }
  }
}