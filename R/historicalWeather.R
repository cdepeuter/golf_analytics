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
    
    # for up to 5 years back, whats the weather like here?
    all.weathers <- lapply(0:5, function(years){
        weeks <- 3
        firstDate <- event[["start"]] - 7 * weeks
        lastDate <- event[["end"]] + 7 * weeks
        # adjust by year
        firstDate <- firstDate - years * 365
        lastDate <- lastDate - years * 365
        
        dates <- seq.Date(firstDate, lastDate, by="day") %>% lapply(getWugDateFormat)
        locString <- paste(event[, c("hole_lat", "hole_lon")], collapse = ",")
        reqs <- paste(locString, dates, sep="-")
        thisYearWeather <- getWeatherObsLocationDates(reqs)
        
        return(thisYearWeather)
    })
    
    
    past.weather <- do.call("rbind", all.weathers)
    
    
    # add year, month date separate columns, group by year, month, 
    days_of_rain <- past.weather %>% mutate( date_of_year=format(date_time, "%j"), year=format(date_time, "%y")) %>% 
                                     group_by(date) %>% 
                                     summarise(rain = sum(precip), 
                                               avg_wind = mean(windSpeed, na.rm=TRUE),
                                               date_num=date_of_year[1], 
                                               yr = year[1])
    
    
    ggsave(paste0("./plots/", event[["tourn"]], "_", event[["season"]],"_precip.png"),  ggplot(days_of_rain, aes(x=date_num, y=rain, group=1)) + geom_line() + facet_grid(yr ~ .) + ggtitle("Historical Daily Rainfall"))
    ggsave(paste0("./plots/", event[["tourn"]], "_", event[["season"]],"_wind.png"),  ggplot(days_of_rain, aes(x=date_num, y=avg_wind, group=1)) + geom_line() + facet_grid(yr ~ .) + ggtitle("Historical Wind"))
    
   
    return(past.weather)
}



