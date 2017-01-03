
#.big in case i want to work on smaller dataset
shotlink.big <- read.csv("./data/shot-2016-pga.txt", sep=";", header=TRUE)
#turn factors into characters
shotlink.big$Date <- as.character(shotlink.big$Date)

#unique shot ids
shotIds <- apply(shotlink.big, 1, getShotId)
shotlink.big$shotId <- shotIds


#get all weather for shots in this file
weather <- getAllWeatherForShots(shotlink.big, pga)


#get weather data for each shot
shotlink.conditions <- apply(shotlink.big, 1, getWeatherForShot, weather)
shotlink.weather <- do.call("rbind", shotlink.conditions)
shotlink.weather$shotId <- shotIds

write.table(shotlink.weather, "./data/shotlink-2016-weather-pga.csv", sep=",", row.names = FALSE)

# 
# 
# #day never teed off on third day, check stricker
# stricker <- shotlink.big[shotlink.big$Player.. == 6527,]
# stricker.conditions <- apply(stricker, 1, getWeatherForShot, weather)
# stricker.Df <- do.call("rbind", stricker.conditions)
# stricker.Df$id <- stricker$shotId
#     
# write.table(stricker.Df, "./data/shot-2016-stricker-weather-pga.csv", sep=",", row.names = FALSE)
