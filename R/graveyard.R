# old scripts, dont think ill need them but keeping here in case they may be useful again
googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'


getESPNEventsForSeason <- function(season){
    
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




getEventAddressESPN <- function(id){
    # given an espn eventId, get the address info for espn event
    # input: id
    # output: data frame with city, state, zipCode, country
    
    baseUrl <- "http://site.api.espn.com/apis/site/v2/sports/golf/leaderboard?event="
    tail <- "&lang=en&region=us"
    
    #make request, format in json, then dataframe using jsonlite::fromJSON
    
    eventJSON.reqUrl <- paste(baseUrl, id, tail, sep="")
    print(paste("making req for tourney", id, eventJSON.reqUrl))
    eventJSON.req <- GET(eventJSON.reqUrl)
    eventJSON <- content(eventJSON.req, as="text")
    eventJSON.obj <- jsonlite::fromJSON(eventJSON)
    
    #get address info from nested data frame
    addrInfo <- eventJSON.obj$events$courses[[1]]$address
    print(addrInfo)
    
    if(is.null(addrInfo)){
        print("NULL info")
        addrInfo <- list()
        addrInfo[["state"]] <- NA
        addrInfo[["city"]] <- NA
        addrInfo[["country"]] <- NA
        addrInfo[["zipCode"]] <- NA
    }
    
    #fill NAs
    if(is.null(addrInfo[["zipCode"]]) || is.na(addrInfo[["zipCode"]]) ){
        addrInfo[["zipCode"]] <- NA
    } else if(nchar(as.character(addrInfo[["zipCode"]])) == 4){
        #if leading 0 is removed we want to add one
        addrInfo$zipCode <- paste0("0", addrInfo[["zipCode"]])
    }
    
    if(is.null(addrInfo[["state"]])){
        addrInfo[["state"]] <- NA
    }
    
    if(is.null(addrInfo[["country"]])){
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
    place <- gsub("G&CC", "Golf and Country Club", place)
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
        jsonlite::fromJSON()
    
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
        } else if(course[["courseName"]] == "TPC San Antonio - AT&T Oaks"){
            place <- "TPC San Antonio"
        } else if(course[["courseName"]] == "TPC Four Seasons Resort"){
            place <- "TPC Four Seasons"
        } else if(course[["courseName"]] == "Chambers Bay GC"){
            place <- "Chambers Bay Golf Course"
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
            jsonlite::fromJSON()
        
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
        jsonlite::fromJSON()
    
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
    if(is.na(course[["zipCode"]]) | nchar(course[["zipCode"]]) == 12){
        place <- course[["courseName"]]
        place <- gsub("G&CC", "Golf and Country Club", place)
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
            jsonlite::fromJSON()
        
        #get placeID of top result
        
        place.placeId <- place.json$predictions$place_id[1]
        
        if(is.null(place.placeId)){
            
            
            print(paste("PLACE ID NOT FOUND:", place.placeId))
            
            #try again use just city and state
            place <- course[["courseName"]]
            
            place <- gsub("Bay GC", "Bay Golf Course", place)
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
                jsonlite::fromJSON()
            
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
                    jsonlite::fromJSON()
                
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
            jsonlite::fromJSON()
        
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


#key for weather underground api calls





getObservationsFromWeather <- function(weatherContent){
    # format the weather observations from json into workable format
    # input: weather underground json observation string
    # output: dataframe of observations
    
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    observations <- weatherJSON$history$observations
    
    # TODO CHECK TIMEZONE DATA
    # maybe use UTC if this is an issue
    if(observations$date$tzname[1] !="America/New_York"){
        print("TIMEZONE NOT STANDARD");
    }
    
    
    #remove nested dataframe and add info in separate columns
    dateData <- observations$date
    hr <- observations$date$hour
    min  <- observations$date$min
    time <- as.integer(paste(hr, min, sep=""))
    
    
    year <- observations$date$year
    month <- observations$date$mon
    day <- observations$date$mday
    
    date <- paste(year, month, day, sep="")
    
    observations$date <- date
    observations$time <- time
    
    #drop utc nested dataframe, other column which is probably meter name, 
    observations <- observations[, !(colnames(observations) %in% c("utcdate", "metar" ))]
    
    return(observations)
}


# this file loads all shotlink files in the data/shotlink/ folder and gets weather info for each shot
# it puts the weather files for each tourney in the data/shotlink/processed folder

# get a table of events for the current season, this is currently hardcoded as 2016
# in loadData.R. With these events, find the event for each shotlink file by date, 
# and using the zip code for that event find the corresponding weather information
process_shotlink <- function(events.us){
    shotlink.directory <- "./data/shotlink"
    allFiles <- list.files(shotlink.directory)
    
    for(f in allFiles){
        # only check for txt files
        if(length(grep(".txt", f)) > 0){
            
            # get all shots
            shots <- read.csv(paste0(shotlink.directory,"/", f), sep=";", header=TRUE)
            # turn factors into characters
            shots$Date <- as.character(shots$Date)
            shots.first <- shots[1,]
            
            # unique shot ids
            shotIds <- apply(shots, 1, getShotId)
            shots$shotId <- shotIds
            
            # get tournament for this event by matching the date of the first shot
            thisTournament <- events.us[which( as.Date(events.us$start) == as.Date(shots.first$Date, format="%m/%d/%Y")),]
            
            
            # make sure zip is 5 chars, TODO fix globally
            zipCode <- thisTournament$zipCode
            if(nchar(as.character(zipCode)) == 4){
                # if leading 0 is removed we want to add one
                zipCode <- paste0("0", as.character(zipCode))
            }
            
            
            # write data to files
            filePrefix <- paste0("./data/shotlink/processed/shotlink-",gsub(" ", "_", thisTournament$label),"-",season)
            weatherFile <- paste0( filePrefix,"-",zipCode,"-weather",".csv")
            shotFile <- paste0(filePrefix,".csv")
            shotsAndWeatherFile <- paste0(filePrefix, "shots_weather.csv" )
            
            # if this tournaments already been processed dont do this again
            if(!file.exists(weatherFile) && !file.exists(shotFile) && !file.exists(shotsAndWeatherFile)){
                # get all weather for shots in this file
                weather <- getAllWeatherForShots(shots, thisTournament)
                
                
                # get weather data for each shot
                shots.conditions <- apply(shots, 1, getWeatherForShot, weather)
                shots.weather <- do.call("rbind", shots.conditions)
                
                #bind tables, write to output
                shots.and.weather <- cbind(shots, shots.weather)
                shots.weather$shotId <- shotIds
                
                write.table(shots.weather, weatherFile, sep=",", row.names = FALSE)
                write.table(shots, shotFile, sep = ",", row.names = FALSE) 
                write.table(shots.and.weather, shotsAndWeatherFile, sep = ",", row.names = FALSE) 
            }
        }
    }
}




do_metawrite <- function(events.us){
    metaWeather <- getWeatherForTournaments(events.us)
    metaWeatherFile <- paste0("./data/meta_weather_us_", season, ".csv")
    
    write.table(metaWeather, metaWeatherFile, sep=",", row.names = FALSE)
}




getAllWeatherForShots <- function(shots, course){
    # for each date in the shotlink data, get the weather observations for the course and store those
    # in memory so when running get weather for shot no API call need to be made
    dates <- unique(shots$Date)
    
    weathers <- list()
    
    for(i in 1:length(dates)){
        weathers[[i]] <- getWeatherResponseForCourseDate( dates[i], course)
    }
    
    names(weathers) <- dates
    
    return(weathers)
}



getAirportLocation <- function(code){
    #given an airport code, first find location, then get lat/log
    place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", paste(code, "Airport", sep="+"), "&key=", googleKey) 
    print(paste("getting airport id, first try", place.url))
    
    place.json  <- place.url %>%
        GET() %>%
        content(as="text") %>%
        jsonlite::fromJSON()
    
    place.placeId <- place.json$predictions$place_id[1]
    
    if(is.null(place.placeId)){
        print("place id not found")
    }else{
        print(paste("PLACE ID FOUND:", place.placeId))
    }
    
    
    #use details api to get lat/log
    
    place.detailsUrl <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey)
    print(paste("getting place details for ", code,place.detailsUrl))
    place.detailsJSON <- place.detailsUrl %>% 
        GET() %>%
        content(as="text") %>%
        jsonlite::fromJSON()
    
    #return lat & long
    place.latLong <- unlist(place.detailsJSON$result$geometry$location)
    
    #if no location do something
    return(place.latLong)
}



#' Get a summary of weather for an event
#'
#' This function finds summary weather data for an individual golf event
#' @param event from getPGAEvents() function
#' @return Dataframe of summary information from event
#' @export
#' @examples
#' getWeatherForTournament()

getWeatherForTournament <- function(course){
    
    # get weather for 3 days before tournament
    dates <- seq.Date(as.Date(course[["start"]])-3, as.Date(course[["end"]]), by="day")
    
    ret <- list()
    ret[1] <- course[["course.1"]]
    ret[2] <- course[["tourn"]]
    
    #getWeatherResponseForCourseDate
    nDates <- length(dates)
    
    #hope 6 is the max number of days in tourney
    for(i in 1:9){
        if(i <= length(dates)){
            date <- dates[i]
            
            
            #file should save, thats what we really want
            #but from this response check out airport code
            wObs <- getWeatherResponseForCourseDate(date, course)
            nObs <- dim(wObs)[1]
            
            ret[i+2] <- nObs
            
            #TODO 5 days uh oh
        }else{
            ret[i+2] <- NA
        }
    }
    
    #keep track of metar/airport code
    air <- " " #substring(strsplit(wObs$metar[1], split = " ")[[1]][[2]], 2)
    paste("airport code", air)
    
    ret[12] <- air
    
    #getAirport location
    #airLoc <- getAirportLocation(air)
    #airloc <- c(1,2)
    #swap lat/long
    #airLoc <- as.vector(c(airLoc[2], airLoc[1]))
    #courseLoc <- as.vector(as.double(c(course[["lng"]], course[["lat"]])))
    
    # #get Distance convert to miles
    # if(is.null(courseLoc) | is.null(airLoc)){
    #     dist <- NA
    # }else{
    #     dist <- distVincentySphere(airLoc, courseLoc) * 0.000621371 
    # }
    # 
    ret[13] <- 4
    # 
    # print(paste("Distance between course and measurements", dist))
    
    return(ret)
}



