#' Get historical weather info for tournament
#'
#' This function finds and saves historical weather info for tournaments
#' @param event event from pga
#' @return saved info
#' @export
#' @import dplyr
#' @import ggplot2
#' @examples
#' historicalWeatherForEvent(safeway)


historicalWeatherForEvent <- function(event){
    # for a given location from an event, get weather for previous 5 years on those dates
    obs_list <- lapply(responses, getObsFromWeatherResp)
    #getWeatherResponseForCoordsDate
    
    this_tourney.weather <- getWeatherObsLocationDates(event[,c("hole_lat", "hole_lon")], event[,c("start", "end")])
    
    
}


