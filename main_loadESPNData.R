

season <- "2016"
#season <- "2015"
filename <- paste0("./data/events_US_latlong_", season, ".csv")

if(file.exists(filename)){
    #load data from file
    events.us <- read.csv(filename)
}else{
    source("./scrape/events.R")
    source("./scrape/elevation.R")
    #file doesnt exist, scrape it
    events <- getESPNEventsForSeason(season)
    
    events.addressInfo <- lapply( events$id, getEventAddressESPN)
    events.addressInfo <- do.call(rbind,events.addressInfo)
    
    #if a tournament has multiple courses (laquinta-2483) itll have duplicate entries
    events.addressInfo <- events.addressInfo[!duplicated(events.addressInfo$id),]
    
    #bind to old df
    events <- cbind(events, events.addressInfo)
    
    #drop bad cols
    events <- events[, !(colnames(events) %in% c("document.id", "array.index", "id.1"))]
    
    #get US events, only grabbing coordinates for those, remove bad events
    events.us <- events[events$country == "United States",]
    events.usKeep <- !apply(events.us, 1, function(x) all(is.na(x)))
    events.us <- events.us[events.usKeep, ]
    
    
    #coerce zip into string so it keeps 5 chars
    events.us$zipCode <- as.character(events.us$zipCode)
    
    #fill missing zip codes
    zips <- apply(events.us, 1, fillMissingZips)
    events.us$zipCode <- zips
    
    #get all lat/long, add to dataframe
    events.uslatlong <- apply(events.us, 1, getLatLongByPlaceName)
    events.us <- cbind(events.us, t(events.uslatlong))
    
    #get year from startDate
    events.us$year <- unlist(lapply(events.us$start, function(x){return(format(as.Date(x), '%Y'))}))
    
    
    
    elevation <- unlist(apply(events.us, 1,getElevationForCourse ))
    events.us$elevation <- elevation
    
    
    #write data to file
    write.table(events, paste0("./data/events_", season, ".csv"), sep=",",eol = ";", row.names = FALSE)
    
    #mac formatting
    write.table(events.us, filename, sep=",", row.names = FALSE)
    
    #mark formatting
    #write.table(events.us, paste0("./data/events_US_latlong-", season, ".csv"), sep=";", row.names = FALSE)
    
}

