source("./scrape/events.R")

pga <- getPGAEventsForSeason("2016")
pga.coord <- apply(pga, 1, getLocationForPGAEvent)


#currently over my pga rate limit, hard code to keep going with weather stuff
rtj <- pga[29,]
rtj[["lat"]] <-32.6789455
rtj[["lng"]] <- -85.4224935

