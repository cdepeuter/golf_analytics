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


historicalWeatherForEvent <- function(event, locString = NA, midDate = NA, yearRange, weeks){
    # for a given location from an event, get weather for previous 5 years on those dates
    
    
    if(!exists("numWeatherRequests")){
        numWeatherRequests <<- 2
        lastWeatherRequest <<- Sys.time()
    }
    filename <- "./data/historical_weather_"
    plotsfile <- "./plots/"
    summaryfilename <- ""

        # for up to 5 years back, whats the weather like here?
    all.weathers <- lapply(0:yearRange, function(years){
        if(!is.na(midDate)){
            firstDate <- as.Date(midDate) - 7 * weeks
            lastDate <-  as.Date(midDate) + 7 * weeks
            # adjust by year
            firstDate <- firstDate - years * 365
            lastDate <- lastDate - years * 365
           
        }else{
            firstDate <- event[["start"]] - 7 * weeks
            lastDate <- event[["end"]] + 7 * weeks
            # adjust by year
            firstDate <- firstDate - years * 365
            lastDate <- lastDate - years * 365
        }
        
        
       if(lastDate >= Sys.Date() - 1){
           print("last date to pull is past current date, use previous year")
           stop()
       }
        
        dates <- seq.Date(firstDate, lastDate, by="day") %>% lapply(getWugDateFormat)
        if(is.na(locString)){
            locString <- paste(event[, c("hole_lat", "hole_lon")], collapse = ",")
            courseString <- gsub(" ", "_", event[, "course.1"])
        }

        if(plotsfile == "./plots/"){
            # update file names on first run
            summaryfilename <<- paste0(filename, "summary_", as.character(firstDate),"_", courseString, ".txt")
            filename <<- paste0(filename, as.character(firstDate),"_", courseString , ".txt")

            plotsfile <<- paste0(plotsfile, as.character(firstDate),"_", courseString)
            
        }
        
        reqs <- paste(locString, dates, sep="-")
        #print(reqs)
        thisYearWeather <- getWeatherObsLocationDates(reqs)
        
        return(thisYearWeather)
    })
    
    
    past.weather <- do.call("rbind", all.weathers)
    
    
    write.table(past.weather, filename, sep=";", row.names = FALSE)
   
     # add year, month date separate columns, group by year, month, 
    days_of_rain <- past.weather %>% mutate( date_of_year=format(date_time, "%j"), year=format(date_time, "%y")) %>% 
                                     group_by(date) %>% 
                                     summarise(rain = sum(precip), 
                                               avg_wind_speed = mean(windSpeed, na.rm=TRUE),
                                               wind_dir_avg = yamartino(windDirDeg),
                                               wind_dir_std = yamartino_std(windDirDeg),
                                               wind_speed_75_pct = quantile(windSpeed, .75),
                                               wind_speed_max = max(windSpeed, na.rm=TRUE),
                                               date_num=date_of_year[1], 
                                               yr = year[1])
    
    
    write.table(days_of_rain, summaryfilename, sep=";", row.names = FALSE)
    ggsave(paste0(plotsfile, "_precip.png"),  ggplot(days_of_rain, aes(x=date_num, y=rain, group=1)) + geom_line() + facet_grid(yr ~ .) +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Historical Daily Rainfall"))
    ggsave(paste0(plotsfile, "_wind.png"),  ggplot(days_of_rain, aes(x=date_num, y=avg_wind_speed, group=1)) + geom_line() + facet_grid(yr ~ .)  +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Historical Wind"))
    
   
    #return(past.weather)
    return(0)
}



