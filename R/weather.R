#' Scrape weather info for all dates in a tournament
#'
#' This grabs the weather response for 3 days before up to the end of a tournament
#' @param event from getPGAEvents() function
#' @return 
#' @export
#' @examples
#' scrapeWeatherForTournament(masters09)

scrapeWeatherForTournament <- function(e){
    # get weather for 3 days before tournament
    dates <- seq.Date(as.Date(e[["start"]])-3, as.Date(e[["end"]]), by="day")
    
    t <- lapply(dates, getWeatherResponseForCourseDate, e)
}


#' Scrape weather info for all tournaments
#'
#' This grabs the weather response for 3 days before up to the end of multiple tournaments
#' respecting the rate limit of the weather underground api
#' @param event from getPGAEvents() function
#' @return 
#' @export
#' @examples
#' scrapeWeatherForTournaments(events)
scrapeWeatherForTournaments <- function(events){
    #scrape weather, sleep so limit is not reached
    apply(events, 1, function(event){
        scrapeWeatherForTournament(event)
        Sys.sleep(60)
    })
}


tournamentWeatherSummary <- function(event){
    dates <- seq.Date(as.Date(event[["start"]])-3, as.Date(event[["end"]]), by="day")
    retinfo <- data.frame(event[["course.1"]])
    whichDate <- -3
    print(length(dates))
    for(i in  1:length(dates)){
        
        date <- dates[i]
        print(date)
        resp <- getWeatherResponseForCourseDate(date, event)
        resp.json <- jsonlite::fromJSON(resp)
        statsIWant <- c("precipi", "maxwspdi", "minwspdi" ,"meanwindspdi", "meanwdird")
        statsRName <- c("precip_inches_day_", "max_wind_mph_day_", "min_wind_mph_day_", "mean_wind_mph_day_", "mean_wind_dir_degrees_day_")
        statsRName <- paste0(statsRName, as.character(whichDate))
        relevantInfo <- resp.json$history$dailysummary[statsIWant]
        colnames(relevantInfo) <- statsRName
        retinfo <- cbind(retinfo, data.frame(relevantInfo, check.names = FALSE))
        retinfo$start_date <- as.Date(event[["start"]])
        whichDate <- whichDate + 1 
        if(whichDate == 0){
            whichDate <- whichDate + 1
        }
        
    }
    
    return(retinfo)
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
    air <- substring(strsplit(wObs$metar[1], split = " ")[[1]][[2]], 2)
    paste("airport code", air)
    
    ret[12] <- air
    
    #getAirport location
    airLoc <- getAirportLocation(air)
    
    #swap lat/long
    airLoc <- as.vector(c(airLoc[2], airLoc[1]))
    courseLoc <- as.vector(as.double(c(course[["lng"]], course[["lat"]])))
    
    #get Distance convert to miles
    if(is.null(courseLoc) | is.null(airLoc)){
        dist <- NA
    }else{
        dist <- distVincentySphere(airLoc, courseLoc) * 0.000621371 
    }
    
    ret[13] <- dist
    
    debug.print(paste("Distance between course and measurements", dist))
    
    return(ret)
}


#' On a given day/course get the weather observations from a local file
#'
#' This function loads the weather json from a local directory and writes summary information for that day
#' @param string input name of course
#' @param string date of obs
#' @return summary info for event
#' @export
#' @examples
#'getWeatherForCourseDate("TPC Scottsdale")



getWeatherForCourseDate <- function(course, dateStr){
    # get weather info json response for event at given address on date
    # input: course info with city, state, zip(maybe)
    # output json response from weather underground api
    
    # DONT run this function in vectorized format on an array
    # weatherUnderground maxes API calls at 10 per minute
    
    #get weather and addr for filename
    wugDateStr <- dateStr
    
    addr <- paste(course[["lat"]], course[["lng"]], sep=",") 
    
    
    filename <- paste0("./data/weather/",addr, "-", wugDateStr, ".json")
    filename <- gsub(" ", "", filename)
    
    
    if(!file.exists(filename)){
        stop("NO LOCAL FILE")
    }
    
    
    weatherContent <- read_file(filename)
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    observations <- weatherJSON$history$observations
    
    # TODO CHECK TIMEZONE DATA
    # maybe use UTC if this is an issue
    if(weatherJSON$history$date$tzname !="America/New_York"){
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


getWeatherForTournaments <- function(courses){
    #get meta weather info for each course/date
    
    ## unfortunate for loop but need to pause requests every minute because of rate limit
    infos <- list()
    for(i in 1:dim(courses)[1]){
        infos[[i]] <- getWeatherForTournament(courses[i,])
        
        #sleep for 10 seconds
        debug.print("Sleeping for 1 minute")
        Sys.sleep(60)
    }
    
    df <- do.call("rbind", infos)
    print(dim(df))
    colnames(df) <- c("course", "tournament","pre3_obs", "pre2_obs", "pre1_obs", "day1_obs", "day2_obs", "day3_obs", "day4_obs", "day5_obs", "day6_obs", "airport_code", "air_course_dist_miles")
    return(df)
}



getDailyDataFromWeatherResp <- function(weatherContent){
    # get relevant info from weather underground
    # input: json format weather string
    # output: field from response in named list
    
    #put json into table
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    
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



getAirportLocation <- function(code){
    #given an airport code, first find location, then get lat/log
    place.url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", paste(code, "Airport", sep="+"), "&key=", googleKey) 
    debug.print(paste("getting airport id, first try", place.url))
    
    place.json  <- place.url %>%
        GET() %>%
        content(as="text") %>%
        jsonlite::fromJSON()
    
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
        jsonlite::fromJSON()
    
    #return lat & long
    place.latLong <- unlist(place.detailsJSON$result$geometry$location)
    
    #if no location do something
    return(place.latLong)
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



getWeatherResponseForCourseDate <- function(dateStr, course){
    # get weather info json response for event at given address on date
    # input: course info with city, state, zip(maybe)
    # output json response from weather underground api
    
    # DONT run this function in vectorized format on an array
    # weatherUnderground maxes API calls at 10 per minute
    
    #get weather and addr for filename
    wugKey <-"8569324e20ad844f"
    wugDateStr <- getWugDateFormat(dateStr)
    addr <- paste(course[["hole_lat"]], course[["hole_lon"]], sep=",") 
    
    if(length(addr) == 0){
        stop("No coordinates for course")
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
    
    
    return(weatherContent)
}




