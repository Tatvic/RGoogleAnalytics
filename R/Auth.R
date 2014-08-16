#' Authorize the RGoogleAnalytics package to the user's Google Analytics 
#' Account using OAuth2.0 
#' 
#' @description
#' This function expects a Client ID and Client Secret. In order to obtain 
#' these, you will have to register an application with the Google Analytics 
#' API. This can be done as follows
#' 
#' - Go to \url{https://console.developers.google.com}
#' 
#' - Create a New Project and enable the Google Analytics API
#' 
#' - On the Credentials screen, create a new Client ID for Application Type 
#' "Installed Application". 
#' 
#' - Copy the Client ID and Client Secret to your R Script as shown in the 
#' Examples section below
#' 
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
#' In subsequent runs, User Consent is not required unless you are querying a 
#' Google Analytics profile associated with a different email account. 
#' This function uses \code{\link[httr]{oauth2.0_token}} under the hood to 
#' create the OAuth Tokens. The Access Token has a 60 minute lifetime after 
#' which it expires and a new token has to be obtained. This can be done using 
#' the \code{\link{ValidateToken}} method
#' 
#' @export  
#' 
#' @return google.token A Token object containing all the data required for 
#' OAuth access. See \code{\link[httr]{Token2.0}} for additional information 
#' on the Token object 
#' 
#' @examples \dontrun{
#' # Generate the oauth_token object
#' oauth_token <- Auth(client.id = "150487456763-XXXXXXXXXXXXXXX.apps.googleusercontent.com",
#' client.secret = "TUXXXXXXXXXXXX_TknUI")
#' 
#' # Save the token object for future sessions
#' save(oauth_token, file="oauth_token")
#' 
#' # Load the token object 
#' load("oauth_token")
#' }
#' 
#' @importFrom httr oauth_app
#' @importFrom httr oauth2.0_token
#' @importFrom httr oauth_endpoints
#' 
Auth <- function(client.id,client.secret) {
  
  # Whether to allow user to save client secret to disk?
  
  myapp <- oauth_app("google", client.id,
                     client.secret)
  google.token <- oauth2.0_token(oauth_endpoints("google"), myapp,
                  scope = "https://www.googleapis.com/auth/analytics.readonly")
  return(google.token)
}