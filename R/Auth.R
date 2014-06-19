#' @title
#' Authorizes the RGoogleAnalytics library to the user's Google Analytics Account 
#' 
#' @description
#' This function expects a Client ID and Client Secret file. In order to obtain this file, you will
#' have to register an app with the Google Analytics API
#' 
#' - Go to \url{https://console.developers.google.com} 
#' 
#' - Create a New Project and enable the Google Analytics API
#' 
#' - On the Credentials screen, create a new Client ID for Application Type "Installed Application". 
#' 
#' - Copy the Client ID and Client Secret to your R Script
#' 
#' 
#' @param client.id Equivalent to a user name
#' @param client.secret Equivalent to a password
#' 
#' 
#' @details
#' When evaluated for the first time this function asks for User Consent
#' for the Google Analytics Account and creates a OAuth Token Object. 
#' The token object can be saved locally to a file on the user's system. 
#' In subsequent executions, a browser redirect is not required.
#' The Access Token has a 60 minute lifetime after which it expires and a new token has to be obtained
#' This function uses \code{httr} under the hood to create the OAuth Tokens.
#'
#' @export  
#' 
#' @return google.token A Token object containing all the data required for OAuth access. See \code{Token2.0} for 
#' additional information on the Token object 
#' 
#' @examples \dontrun{
#' # Generate the oauth_token object
#' oauth_token <- Auth(client.id = "150487456763-XXXXXXXXXXXXXXX.apps.googleusercontent.com",
#' client.secret = "TUXXXXXXXXXXXX_TknUI")
#' # Save the token object for future sessions
#' save(oauth_token,file="oauth_token")
#' # Load the token object 
#' load("oauth_token")
#' }
#' 
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' 
Auth <- function(client.id,client.secret) {
  
  # Whether to allow user to save client secret to disk?
  
  myapp <- oauth_app("google", client.id,
                     client.secret)
  google.token <- oauth2.0_token(oauth_endpoints("google"),myapp,
                                 scope = "https://www.googleapis.com/auth/analytics.readonly")
  return(google.token)
}