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
Auth <- function(client.id,client.secret) {
  
  myapp <- oauth_app("google", client.id,
                     client.secret)
  google.token <- oauth2.0_token(oauth_endpoints("google"),myapp,
                                 scope = "https://www.googleapis.com/auth/analytics.readonly")
  return(google.token)
}