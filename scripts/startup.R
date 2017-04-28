shots17 <- getShotlinkExtTable("shot-ext-2017.txt")
shots17$shot_degrees <- unlist(apply(shots17, 1, getShotDegrees))


events <- getPGAEvents()

events <- getLocationForEvents(events)



sanderson <- events[262,]
shriners <- events[263,]


sanderson.weather <- getWeatherObsForTournament(sanderson)
shriners.weather <- getWeatherObsForTournament(shriners)

#safeway.shots <- shots17[shots17$perm_tourn == 464,]
#sanderson.shots <- shots17[shots17$perm_tourn == 54,]
#shriners.shots <- shots17[shots17$perm_tourn == 47,]


# rainy tournaments

safeway <- events[261,]
safeway.shots <-  getShotlinkExtTable("shot-ext-tournament-2017-552.txt", safeway$local_tz)
safeway.weather <- getWeatherObsLocationDates(safeway[,c("hole_lat", "hole_lon")], safeway[,c("start", "end")])
safeway.shot_weather <- matchWeatherToShots(safeway.shots, safeway.weather)
safeway.drives <- filterShots(safeway.shot_weather)

pga_2016 <- events[253,]
pga_2016.weather <- getWeatherObsForTournament(pga_2016)

doral <- events[235,]
doral.shots <- getShotlinkExtTable("shot-ext-doral-2016.txt", doral$local_tz)
doral.weather <- getWeatherObsForTournament(doral)