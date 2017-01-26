source("./scrape/events.R")
source("./scrape/weather.R")
seasons <- c("2015","2016")
seasons <- c("2014")

for(season in seasons){
    
    pga <- getPGAEventsForSeason(season)
    weathers <- getWeatherForTournaments(pga)
    
    

    if(!("lat" %in% colnames(pga))){
        # add coordinates for course since we dont have them
        pga.coord <- apply(pga, 1, getLocationForPGAEvent)
        pga <- cbind(pga, t(pga.coord))
        write.table(pga, paste0("./data/event_course_date/events_",season,"_latlong.csv"), row.names = FALSE)
    }
  
    
    write.table(weathers, paste0("./data/meta_weather_pga_", season, ".csv"), row.names = FALSE)
}
