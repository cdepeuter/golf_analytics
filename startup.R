shots17 <- getShotlinkExtTable("shot-ext-2017.txt")
shots17$shot_degrees <- unlist(apply(shots17, 1, getShotDegrees))


events <- getPGAEvents()
events <- getLocationForEvents(events)


safeway <- events[261,]
sanderson <- events[262,]
shriners <- events[263,]

safeway.weather <- getWeatherObsForTournament(safeway)
sanderson.weather <- getWeatherObsForTournament(sanderson)
shriners.weather <- getWeatherObsForTournament(shriners)

safeway.shots <- shots17[shots17$perm_tourn == 464,]
sanderson.shots <- shots17[shots17$perm_tourn == 54,]
shriners.shots <- shots17[shots17$perm_tourn == 47,]