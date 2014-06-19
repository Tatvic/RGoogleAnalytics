#' This function will do the parsing operation on the JSON reponse 
#' returned from the Google Management API and return the
#' dataframe stored with the profile id and profile name
#' @keywords internal 
#' @param api.reponse.json The JSON response from GetProfileData function which will request to the Google Management API.                      
#' @return Profileres.list The list stored with totalResults as value of the total available data rows and profiles as the R dataframe object with two columns as column id and column name.
#'   
GetProfilesFromJSON <- function(api.response.json) {
                              
  GA.profiles <- ParseApiErrorMessage(api.response.json)
  TotalProfiles <- GA.profiles$totalResults
  if (!is.null(GA.profiles$code)) {
    stop(paste("code: ",
               GA.profiles$code,
               "Reason: ",
               GA.profiles$message))
  }
  
  GA.profiles.param <- t(sapply(GA.profiles$items,
                                '[',
                                1 : max(sapply(GA.profiles$items, length)))) 
  profiles.id <- as.character(GA.profiles.param[, 1])
  profiles.name <- as.character(GA.profiles.param[, 7])
  if (length(profiles.id) == 0) {
    stop("Please check the access token. It may be invalid or expired")
  } else {
    profiles <- data.frame(id = profiles.id,
                           name = profiles.name,
                           stringsAsFactors = FALSE)
    profileres.list <- list(totalResults = TotalProfiles,
                            profiles = profiles)
    return(profileres.list)     
  }
}