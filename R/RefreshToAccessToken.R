  #' This function takes the Refresh Token as an argument and retrives a new 
  #' Access Token based on it
  #' Reference : https://developers.google.com/accounts/docs/OAuth2#installed
  #' @param refresh.token  Refresh Token that was saved to the local file
  #' @param client.id      Client ID of the Application. This is a OAuth2.0 Credential   
  #' @param client.secret  Client Secret of the Application. Again this too is an
  #'                   OAuth2.0 Credential   
  #'   
  #' @return Access Token   New Access Token
  #'   
  #' @author Vignesh Prajapati 
  RefreshToAccessToken <- function(refresh.token, client.id, client.secret){
    
    refresh.token.list = fromJSON(postForm('https://accounts.google.com/o/oauth2/token',
                                           refresh_token = refresh.token,
                                           client_id = client.id,
                                           client_secret = client.secret,
                                           grant_type = "refresh_token",
                                           style = "POST" ))
    
    return(refresh.token.list$access_token)
  }