# this file loads all shotlink files in the data/shotlink/ folder and gets weather info for each shot
# it puts the weather files for each tourney in the data/shotlink/processed folder

# get a table of events for the current season, this is currently hardcoded as 2016
# in loadData.R. With these events, find the event for each shotlink file by date, 
# and using the zip code for that event find the corresponding weather information
source("./loadData.R")
source("./utils.R")

source("./scrapeWeather.R")

shotlink.directory <- "./data/shotlink"
allFiles <- list.files(shotlink.directory)

for(f in allFiles){
    if(length(grep(".txt", f)) > 0){
        
        
        #get all shots
        shots <- read.csv(paste0(shotlink.directory,"/", f), sep=";", header=TRUE)
        #turn factors into characters
        shots$Date <- as.character(shots$Date)
        shots.first <- shots[1,]
        
        #unique shot ids
        shotIds <- apply(shots, 1, getShotId)
        shots$shotId <- shotIds
        
        #get tournament for this event by matching the date of the first shot
        thisTournament <- events.us[which( as.Date(events.us$start) == as.Date(shots.first$Date, format="%m/%d/%Y")),]
        
        #get all weather for shots in this file
        weather <- getAllWeatherForShots(shots, thisTournament)
        
        
        #get weather data for each shot
        shots.conditions <- apply(shots, 1, getWeatherForShot, weather)
        shots.weather <- do.call("rbind", shots.conditions)
        shots.weather$shotId <- shotIds
        
        
        #write data to files
        filePrefix <- paste0("./data/shotlink/processed/shotlink-",gsub(" ", "_", thisTournament$label),"-",season)
        weatherFile <- paste0( filePrefix,"-",thisTournament$zipCode,"-weather",".csv")
        shotFile <- paste0(filePrefix,".csv")
        write.table(shots.weather, weatherFile, sep=",", row.names = FALSE)
        write.table(shots, shotFile, sep = ",", row.names = FALSE)
    }
}







# 
# 
# #day never teed off on third day, check stricker
# stricker <- shots[shots$Player.. == 6527,]
# stricker.conditions <- apply(stricker, 1, getWeatherForShot, weather)
# stricker.Df <- do.call("rbind", stricker.conditions)
# stricker.Df$id <- stricker$shotId
#     
# write.table(stricker.Df, "./data/shot-2016-stricker-weather-pga.csv", sep=",", row.names = FALSE)
