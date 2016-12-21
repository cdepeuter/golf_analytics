#first grabbing all tournaments for given season
season <- "2016"
googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'

getEventsForSeason <- function(season){
  
  #get all season data, turn that into json
  tourUrl <- "http://site.api.espn.com/apis/site/v2/sports/golf/pga/tourschedule?season="
  seasonReq <- GET(paste(tourUrl, season, sep=""))
  seasonJSON <- content(seasonReq, as="text")
  
  
  #get all seasons from nested table
  seasons = seasonJSON %>% 
    enter_object("seasons") %>% 
    gather_array() %>% 
    spread_values("year" = jnumber("year"), "name" = jstring("displayName"))
  
  
  #get all tournaments for the given season in a dataframe
  events <- seasonJSON %>% 
    enter_object("seasons") %>% 
    gather_array() %>% 
    enter_object("events") %>% 
    gather_array %>% 
    spread_values("label" = jstring("label"), "id" = jnumber("id"), "link" = jstring("link"), "start" = jstring("startDate"), "end" = jstring("endDate"))
  
  
  #TODO turn start and end dates from here into date range so you can automatically grab weather for each date
  return(events)
}

events <- getEventsForSeason("2016")

getEventAddress <- function(id){
  # given an espn eventId, get the address info for espn event
  # input: id
  # output: data frame with city, state, zipCode, country
  
  baseUrl <- "http://site.api.espn.com/apis/site/v2/sports/golf/leaderboard?event="
  tail <- "&lang=en&region=us"
  
  #make request, format in json, then dataframe using fromJSON
  print(paste("making req for tourney", id))
  eventJSON.req <- GET(paste(baseUrl, id, tail, sep=""))
  eventJSON <- content(eventJSON.req, as="text")
  eventJSON.obj <- fromJSON(eventJSON)
  
  #get address info from nested data frame
  addrInfo <- eventJSON.obj$events$courses[[1]]$address
  print(addrInfo)
  #fill NAs
  if(is.null(addrInfo$zipCode)){
    addrInfo[["zipCode"]] <- NA
  }
  
  if(is.null(addrInfo$state)){
    addrInfo[["state"]] <- NA
  }
  
  if(is.null(addrInfo$country)){
    addrInfo[["country"]] <- NA
  }
  
  #add course name for places api search
  addrInfo[["courseName"]] <- eventJSON.obj$events$courses[[1]]$name
  
  #add id for remapping
  addrInfo[["id"]] <- id
  
  
  return(data.frame(addrInfo))
}

events.addressInfo <- lapply( events$id, getEventAddress)
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


#get coordinates for each course
getLatLongByPlaceName <- function(course){
  # get coordinates for googles first guess of the course
  # input: course row with courseName, maybe state & city
  # output: latitude & longitude of given course
  
  place <- course[["courseName"]]
  
  
  place <-  gsub(" ", "+", place)

  #use autocomplete API to guess course based on name
  
  print(paste("getting place id for", place))
  place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
  
  place.json  <- place.url %>%
    GET() %>%
    content(as="text") %>%
    fromJSON()
  
  if(length(place.json$predictions) == 0){
    #remove ( ) from string, add town + state, retry req
    
    #if city + state available, add them
    if(!is.null(course[["city"]]) && !is.null(course[["state"]])){
      place <- paste(place, course[["city"]], course[["state"]], sep="+")
    }
    
    place <-  gsub(" ", "+", place)
    
    place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
    place.url <- gsub("\\(.*\\)", "", place.url)
    
    place.json  <- place.url %>%
      GET() %>%
      content(as="text") %>%
      fromJSON()
  }
  
  #get placeID of top result
  
  place.placeId <- place.json$predictions$place_id[1]
  print(paste("PLACE ID FOUND:", place.placeId))
  #https://maps.googleapis.com/maps/api/place/details/json?placeid=ChIJe8Or01Y25IgRee9NrjQN220&key=AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg
  
  #use details api to get lat/log
  print(paste("getting place details for ", place))
  place.detailsJSON <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey) %>% 
    GET() %>%
    content(as="text") %>%
    fromJSON()
  
  #return lat & long
  place.latLong <- unlist(place.detailsJSON$result$geometry$location)

  #fill NA
  if(is.null(place.latLong)){
    place.latLong <- c()
    place.latLong[["lat"]] <- NA
    place.latLong[["lng"]] <- NA
  }

  return(place.latLong)
}



#get all lat/long, add to dataframe
events.uslatlong <- apply(events.us, 1, getLatLongByPlaceName)
events.us <- cbind(events.us, t(events.uslatlong))

#get year from startDate
events.us$year <- lapply(events.us$start, function(x){return(format(as.Date(x), '%Y'))})

#write data to file
write.table(events, paste0("./data/events_", season, ".csv"), sep=",",eol = ";", row.names = FALSE)
write.table(events.us, paste0("./data/events_US_latlong-", season, ".csv"), sep=",",eol = ";", row.names = FALSE)

