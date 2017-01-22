#key for weather underground api calls
wugKey <-"8569324e20ad844f"


getWugDateFormat <- function(dateStr){
    #put file in YYYYMMDD format
    
    #shotlink format "%m/%d/%Y"
    # TODO make this more thorogugh
    if(nchar(dateStr) == 10){
        date <- as.Date(dateStr, format="%m/%d/%Y")
        if(is.na(date)){
            date <- as.Date(dateStr)   
        }
    } else{
        date <- as.Date(dateStr)
    }
    
    return(as.character(date, format = "%Y%m%d"))
}

getWeatherForTournament <- function(course){
    #get all dates for tourney
    # commented out is for ESPN col names
    #debug.print(paste("weather for tourney", course[["label"]]))
    debug.print(paste("weather for tourney", course[["tourn"]]))
    
    # get weather for 3 days before tournament
    dates <- seq.Date(as.Date(course[["start"]])-3, as.Date(course[["end"]]), by="day")
    
    ret <- list()
    #ret[1] <- course[["courseName"]]
    #ret[2] <- course[["label"]]
    ret[1] <- course[["course.1"]]
    ret[2] <- course[["tourn"]]
    
    #getWeatherObservationsForCourseDate
    nDates <- length(dates)
    
    #hope 6 is the max number of days in tourney
    for(i in 1:9){
        if(i <= length(dates)){
            date <- dates[i]
            
            
            #file should save, thats what we really want
            #but from this response check out airport code
            wObs <- getWeatherObservationsForCourseDate(course, date)
            nObs <- dim(wObs)[1]
            
            ret[i+2] <- nObs
            
            #TODO 5 days uh oh
        }else{
            ret[i+2] <- NA
        }
    }
    
    #keep track of metar/airport code
    air <- substring(strsplit(wObs$metar[1], split = " ")[[1]][[2]], 2)
    debug.print(paste("airport code", air)) 
    
    ret[9] <- air
    
    #getAirport location
    airLoc <- getAirportLocation(air)
    
    #swap lat/long
    airLoc <- as.vector(c(airLoc[2], airLoc[1]))
    courseLoc <- as.vector(as.double(c(course[["lng"]], course[["lat"]])))
    
    #get Distance convert to miles
    debug.print(courseLoc)
    debug.print(airLoc)
    if(is.null(courseLoc) | is.null(airLoc)){
        dist <- NA
    }else{
        dist <- distVincentySphere(airLoc, courseLoc) * 0.000621371 
    }
    
    ret[10] <- dist
    
    debug.print(paste("Distance between course and measurements", dist))
    
    return(ret)
}

getWeatherForTournaments <- function(courses){
    #get meta weather info for each course/date
    
    infos <- apply(courses, 1, getWeatherForTournament)
    df <- do.call("rbind", infos)
    colnames(df) <- c("course", "tournament", "day1_obs", "day2_obs", "day3_obs", "day4_obs", "day5_obs", "day6_obs", "airport_code", "air_course_dist_miles")
    return(df)
}

getWeatherObservationsForCourseDate <- function(course, dateStr){
    # get weather info json response for event at given address on date
    # input: course info with city, state, zip(maybe)
    # output json response from weather underground api
    
    # DONT run this function in vectorized format on an array
    # weatherUnderground maxes API calls at 10 per minute
    
    #get weather and addr for filename
    wugDateStr <- getWugDateFormat(dateStr)
    
    #take spaces out of city
    if(!is.null(course[["lat"]]) && !is.null(course[["lng"]])){
       addr <- paste(course[["lat"]], course[["lng"]], sep=",") 
    } else if(!is.null(course[["zipCode"]])){
        addr <- course[["zipCode"]]
        if(nchar(as.character(addr)) == 4){
            #if leading 0 is removed we want to add one
            addr <- paste0("0", as.character(addr))
        }
    }else if(!is.null(course[["city"]]) && !is.null(course[["state"]])){
        city <- course[["city"]]
        city <- gsub(" ", "_", city)
        addr <- paste(course[["state"]], "/", city)
    }else{
        return("Not enough info to fetch weather")
    }
    
    
    filename <- paste0("./data/weather/",addr, "-", wugDateStr, ".json")
    filename <- gsub(" ", "", filename)

    if(file.exists(filename)){
        debug.print(paste("getting", filename, "locally"))
        weatherContent <- read_file(filename)
    }else{
        #no weather locally, grab file and save it
        
        wugUrl <- paste("http://api.wunderground.com/api/", wugKey,"/history_",wugDateStr, "/q/", addr, ".json", sep = "")
        wugUrl <- gsub(" ", "", wugUrl)
        debug.print(paste("getting weather info ", wugUrl))
        weatherReq <- GET(wugUrl)
        weatherContent <- content(weatherReq, as="text")
        debug.print(paste("saving weather to file", filename))
        write_file(weatherContent, filename)
    }
    
    
    weatherJSON <- fromJSON(weatherContent)
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
    observations <- observations[, !(colnames(observations) %in% c("utcdate" ))]
    
    return(observations)
}


getDailyDataFromWeatherResp <- function(weatherContent){
  # get relevant info from weather underground
  # input: json format weather string
  # output: field from response in named list
  
  #put json into table
  weatherJSON <- fromJSON(weatherContent)
  
  dailySummary <- weatherJSON$history$dailysummary
  
  meanWindSpeed <- dailySummary$meanwindspdi
  rain <- dailySummary$precipi
  minTemp <- dailySummary$mintempi
  maxTemp <- dailySummary$maxtempi
  
  #format data to return
  weatherData <- c(meanWindSpeed, rain, minTemp, maxTemp)
  names(weatherData) <- c("mean Wind", "rain", "min temp", "max temp")
  
  return(weatherData)
}


getObservationsFromWeather <- function(weatherContent){
    # format the weather observations from json into workable format
    # input: weather underground json observation string
    # output: dataframe of observations
    
    weatherJSON <- fromJSON(weatherContent)
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

getAllWeatherForShots <- function(shots, course){
    # for each date in the shotlink data, get the weather observations for the course and store those
    # in memory so when running get weather for shot no API call need to be made
    dates <- unique(shots$Date)
    
    weathers <- list()
    
    for(i in 1:length(dates)){
        weathers[[i]] <- getWeatherObservationsForCourseDate(course, dates[i])
    }
    
    names(weathers) <- dates
    
    return(weathers)
}


getWeatherForShot <- function(shot, weather){
    # for a given shot, find the weather observation with the closest time
    # assuming the observations are at the right zip code and date
    # input: shot in shotlink format, observations in weatherUnderground format
    # output: rain, wind data
    
    
    observations <- weather[shot[["Date"]]][[1]]
    closestObservation <- observations[which.min(abs(observations$time - as.integer(shot[["Time"]]))),] 
    
    dataWeWant <- c("tempi", "hum", "wdird","wdire", "wgusti","precipi","rain", "conds", "time")
    dat <- closestObservation[,dataWeWant]
    colnames(dat) <- c("tempF", "humidity", "wDirDeg","wDir", "windGust","precip","rain", "conds", "weatherTime")
    return(dat)
}



getAirportLocation <- function(code){
    #given an airport code, first find location, then get lat/log
    place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", paste(code, "Airport", sep="+"), "&key=", googleKey) 
    debug.print(paste("getting airport id, first try", place.url))
    
    place.json  <- place.url %>%
        GET() %>%
        content(as="text") %>%
        fromJSON()
    
    place.placeId <- place.json$predictions$place_id[1]
    
    if(is.null(place.placeId)){
        debug.print("place id not found")
    }else{
        debug.print(paste("PLACE ID FOUND:", place.placeId))
    }
    
    
    #use details api to get lat/log
    
    place.detailsUrl <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",place.placeId,"&key=", googleKey)
    debug.print(paste("getting place details for ", code,place.detailsUrl))
    place.detailsJSON <- place.detailsUrl %>% 
        GET() %>%
        content(as="text") %>%
        fromJSON()
    
    #return lat & long
    place.latLong <- unlist(place.detailsJSON$result$geometry$location)
    
    #if no location do something
    return(place.latLong)
}


