args <- commandArgs()

setwd("~/code/golfAnalysis")
#library(golfAnalysis, lib.loc=file.path("~/code/golfAnalysis"))

#library(devtools)
#print("getting package from github")
#install_github("cdepeuter/golf_analytics")
#library(golfAnalysis)


# TODO hack remove

library(chron)
library(dplyr)
library(httr)
library(readr)

source("./R/getPGAEvents.R")
source("./R/shots.R")
source("./R/weather.R")
source("./R/getLocationForEvents.R")
source("./R/getLocationForPGACourse.R")
source("./R/getUrlResponse.R")


tourn_id <- as.integer(args[6])
season <- as.integer(args[7])
print(tourn_id)
print(season)

events <- getPGAEvents()
events <- getLocationForEvents(events)

event <- events[events$perm_tourn == tourn_id & events$season == season,]


print(paste("weather info for tournament", event$tourn))
if(season == 2016){
    event.shots <- getShotlinkExtTable("shot-ext-2016.txt") 
}else if(season == 2017){
    event.shots <- getShotlinkExtTable("shot-ext-2017.txt")
}

event.shots <- event.shots[event.shots$perm_tourn == tourn_id, ]

event.weather <- getWeatherObsForTournament(event)
event.shot_weather <- matchWeatherToShots(event.shots, event.weather)

#write to file
file_name <- paste0("./weather_shots/", gsub(" ", "_",event$tourn), season, ".txt")
print(file_name)
write.table(event.shot_weather, file_name,  sep = ",", row.names = FALSE)




