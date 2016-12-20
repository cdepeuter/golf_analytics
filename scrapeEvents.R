#first grabbing all tournaments for given season
tourUrl <- "http://site.api.espn.com/apis/site/v2/sports/golf/pga/tourschedule?season="
season <- "2016"

#get all season data, turn that into json
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
  
  #add id for remapping
  addrInfo[["id"]] <- id
  
  return(data.frame(addrInfo))
}

events.addressInfo<- lapply( events$id, getEventAddress)
events.addressInfo <- do.call(rbind,events.addressInfo)

#if a tournament has multiple courses (laquinta-2483) itll have duplicate entries
events.addressInfo <- events.addressInfo[!duplicated(events.addressInfo$id),]

#bind to old df
events <- cbind(events, events.addressInfo)

#drop bad cols
events <- events[, !(colnames(events) %in% c("document.id", "array.index"))]

#write to file
write.table(events, paste0("./data/events_", season, ".csv"), sep=",")

