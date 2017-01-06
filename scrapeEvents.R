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
  } else if(nchar(as.character(addrInfo$zipCode)) == 4){
      #if leading 0 is removed we want to add one
      addrInfo$zipCode <- paste0("0", addrInfo$zipCode)
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
    place <- gsub("CC", "Country Club", place)
    
    
    #if city + state available, add them
    if(!is.null(course[["city"]]) && !is.null(course[["state"]])){
        place <- paste(place, course[["city"]], course[["state"]], sep="+")
    }
    
    place <-  gsub(" ", "+", place)
    
    place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
    place.url <- gsub("\\(.*\\)", "", place.url)
    debug.print(paste("getting place id, first try", place.url))
    
    place.json  <- place.url %>%
        GET() %>%
        content(as="text") %>%
        fromJSON()
  
    if(length(place.json$predictions) == 0){
        #remove ( ) from string, add town + state, retry req
        
        #have manual substitutions for the courses that just dont work
        
        
        place <- course[["courseName"]]
        
        place <- gsub("GC", "Golf Club", place)
        place <- gsub("CC", "Country Club", place)
        
        
        if(course[["courseName"]] == "Plantation Course at Kapalua"){
            place <- "Kapalua%20Golf%20-%20The%20Plantation%20Course"
        } else if(course[["courseName"]] == "RTJ Trail (Grand National)"){
            place <- "Grand National Golf"
        }
        #tpc four seasons just need to remove resort
        #tpc san antonio remove at&t oaks
        
        place <- gsub(" ", "+", place)
        
        
        debug.print(paste("no results, try without city and state", place.url))
        
        #use autocomplete API to guess course based on name
        debug.print(paste("getting place id for", place))
        place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
        place.url <- gsub("\\(.*\\)", "", place.url)
        debug.print(place.url)
        
        place.json  <- place.url %>%
          GET() %>%
          content(as="text") %>%
          fromJSON()
      
    }
    
    #get placeID of top result
    
    place.placeId <- place.json$predictions$place_id[1]
    
    if(is.null(place.placeId)){
        debug.print("place id not found")
    }else{
       debug.print(paste("PLACE ID FOUND:", place.placeId))
    }
    
    
    #use details api to get lat/log
    
    place.detailsUrl <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey)
    debug.print(paste("getting place details for ", place, place.detailsUrl))
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

fillMissingZips <- function(course){
    # search google for the golf course, add zip code
    if(is.na(course[["zipCode"]])){
        place <- course[["courseName"]]
        
        place <- gsub("GC", "Golf Club", place)
        place <- gsub("CC", "Country Club", place)
        place <- paste(place, course[["city"]], course[["state"]], sep="+")
        place <- gsub(" ", "+", place)
        
        #use autocomplete API to guess course based on name
        debug.print(paste("getting place id for", place))
        place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
        place.url <- gsub("\\(.*\\)", "", place.url)
        debug.print(place.url)
        
        place.json  <- place.url %>%
            GET() %>%
            content(as="text") %>%
            fromJSON()
        
        #get placeID of top result
        
        place.placeId <- place.json$predictions$place_id[1]
        
        if(is.null(place.placeId)){
            
            
            print(paste("PLACE ID NOT FOUND:", place.placeId))
            
            #try again use just city and state
            place <- course[["courseName"]]
            place <- gsub("GC", "Golf Club", place)
            place <- gsub("CC", "Country Club", place)
            place <- gsub(" ", "+", place)
            
            #use autocomplete API to guess course based on name
            debug.print(paste("getting place id for", place))
            place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
            place.url <- gsub("\\(.*\\)", "", place.url)
            debug.print(place.url)
            
            place.json  <- place.url %>%
                GET() %>%
                content(as="text") %>%
                fromJSON()
            
            #get placeID of top result
            
            place.placeId <- place.json$predictions$place_id[1]
            
            if(is.null(place.placeId)){
                #still null? try just city state name
                
                print(paste("PLACE ID NOT FOUND:", place.placeId))
                
                #try again use just city and state
                place <- paste(course[["city"]], course[["state"]], sep="+")
                place <- gsub(" ", "+", place)
                
                #use autocomplete API to guess course based on name
                debug.print(paste("getting place id for", place))
                place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", place, "&key=", googleKey) 
                place.url <- gsub("\\(.*\\)", "", place.url)
                debug.print(place.url)
                
                place.json  <- place.url %>%
                    GET() %>%
                    content(as="text") %>%
                    fromJSON()
                
                #get placeID of top result
                
                place.placeId <- place.json$predictions$place_id[1]
                print(paste("newid", place.placeId))

            }
        }else{
            print(paste("PLACE ID FOUND:", place.placeId))
        }
        
        
        #use details api to get lat/log
        
        place.detailsUrl <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey)
        print(paste("getting place details for ", place, place.detailsUrl))
        place.detailsJSON <- place.detailsUrl %>% 
            GET() %>%
            content(as="text") %>%
            fromJSON()
        
        addressComponents <- place.detailsJSON$result$address_components
        zip <- addressComponents[which(addressComponents$types == "postal_code"),]$long_name
        print(zip)
    } else{
        zip <- course[["zipCode"]]
        if(nchar(trimws(zip)) == 4){
            #if leading 0 is removed we want to add one
            zip <- paste0("0", trimws(zip))
        }
    }
   
    
    return(zip)
}


