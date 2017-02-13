#' Get location for pga events 
#'
#' This function takes a dataframe of events and returns a dataframe with lat and longituded 
#' binded to the original dataframe
#' @param dataframe evnts
#' @keywords pga golf
#' @return Data Frame of events with coordinates
#' @export
#' @examples getLocationForEvents(events)
#' 
#' 

getLocationForEvents <- function(evnts){
    coords <- lapply(evnts$course.1, getLocationForPGACourse)
    coords <- do.call("rbind", coords)
    return(cbind(events, coords))
}