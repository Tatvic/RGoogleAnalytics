#' This function checks whether the Access Token stored in the local file is 
#' expired. If yes, it generates a new Access Token and updates the local file.
#' If no, then it returns the stored Access Token
#'  
#' @export
#'  

ValidateToken <- function() {
  
  
  load(file.path(path.package("RGoogleAnalytics"),
                 "accesstoken.rda"))
  
  api.response.json <- getURL(paste0("https://www.googleapis.com/oauth2/v1/",
                                     "tokeninfo?access_token=",
                                     token.list$access_token
  ))
  api.response.list <- fromJSON(api.response.json, method = 'C')  
  check.token.param <- regexpr("error", api.response.json)
  
  if (check.token.param[1] != -1) {
    # If token has expired Generate a New Access token
    cat("Access Token had expired. Regenerating access token\n") 
    GenerateAccessToken()
    return(invisible())
  }    
}