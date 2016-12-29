shotlink.big <- read.csv("./data/shot-2016-pga.txt", sep=";", header=TRUE)
#use john daly/jason day for testing
daly <- shotlink.big[shotlink.big$Player.. == 1249,]
day <- shotlink.big[shotlink.big$Player.. == 28089,]


#turn factors into characters
shotlink.big$Date <- as.character(shotlink.big$Date)

#get weather for shotlink shots, make sure its working with daly shots
weather <- getAllWeatherForShots(shotlink.big, pga)
dalyConditions <- apply(daly, 1, getWeatherForShot, weather)
dalyDf <- do.call("rbind", dalyConditions)
daly <- cbind(daly, dalyDf)

#same for day, was raining on third day
day.conditions <- apply(day, 1, getWeatherForShot, weather)
day.Df <- do.call("rbind", day.conditions)
day <- cbind(day, day.Df)


#day never teed off on third day, check stricker
stricker <- shotlink.big[shotlink.big$Player.. == 6527,]
stricker.conditions <- apply(stricker, 1, getWeatherForShot, weather)
stricker.Df <- do.call("rbind", stricker.conditions)
stricker <- cbind(stricker, stricker.Df)
    
write.table(day, "./data/shot-2016-day-weather-pga.csv", sep=",", row.names = FALSE)