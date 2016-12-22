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


#get coordinates for each course
getLatLongByPlaceName <- function(course){
  # get coordinates for googles first guess of the course
  # input: course row with courseName, maybe state & city
  # output: latitude & longitude of given course
  
  place <- course[["courseName"]]
  
  place <- gsub("GC", "Golf Club", place)
  place <- gsub(" ", "+", place)
  

  #use autocomplete API to guess course based on name
  
  debug.print(paste("getting place id for", place))
  place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
  debug.print(place.url)
  
  place.json  <- place.url %>%
    GET() %>%
    content(as="text") %>%
    fromJSON()
  
  if(length(place.json$predictions) == 0){
    #remove ( ) from string, add town + state, retry req
    debug.print(paste("no results, add city and state", course[["city"]], course[["state"]]))
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

  #use details api to get lat/log
  
  place.detailsUrl <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey)
  print(paste("getting place details for ", place, place.detailsUrl))
  place.detailsJSON <- place.detailsUrl %>% 
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

