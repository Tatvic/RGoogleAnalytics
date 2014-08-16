#' To check whether the returned JSON response is error or not. 
#' If it is error then it will  
#' @keywords internal 
#' 
#' @param  api.response.json The json data as reposnse returned by the Google Data feed API or Google Management API   
#' 
#' @description
#' If there is an error in JSON response then this function will return the related error code and message for that error. 
#'
#' @importFrom httr content
ParseApiErrorMessage <- function(api.response.json) {
      
  api.response.list <- content(api.response.json,as="parsed")  
  check.param <- regexpr("error", api.response.list)
  if (check.param[1] != -1) {
    return(list(code = api.response.list$error$code,
                message = api.response.list$error$message))
  } else {
    code <- NULL
    return(api.response.list)
  }   
}