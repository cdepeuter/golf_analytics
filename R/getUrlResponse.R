#' Get response from url
#'
#' This function returns the string of a url response
#' @param string url
#' @return string response
#' @export
#' @examples
#' getUrlResponse("https://www.pga.com")


getUrlResponse <- function(url){
    # input : url
    # output html repsponse for 
    html <- url %>%
        GET() %>%
        content(as="text")
    return(html)
}