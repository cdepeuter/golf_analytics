
print("getting package from github")
library(devtools)
library(httr)
with_config(use_proxy(url = "23.89.158.215", port = 3128), install_github("cdepeuter/golf_analytics"))
#install_github("cdepeuter/golf_analytics")
library(golfAnalysis)


numWeatherRequests <<- 0
args <- commandArgs()
season <- as.integer(args[6])
course <- as.integer(args[7])
loc <- NA
date_ <- NA

print(paste("params", season, course))
if((season == 0) & (course == 0)){
    # use lat and lon
    loc <- as.integer(args[8])
    date_ <- as.integer(args[9])
    
    print(paste("getting historical weather for ", loc, date_))
    historicalWeatherForEvent(NA, loc, date_)
    
}else{
    print(paste("getting historical weather for season", season, "course", course))
    events <- getPGAEvents()
    events <- getLocationForEvents(events)
    
    thisEvent <- events[(events$season == season) & (events$course) == course,]
    print("event found:")
    print(thisEvent)
    historicalWeatherForEvent(thisEvent)
    
}




