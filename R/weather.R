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
    dates <- seq.Date(as.Date(e[["start"]])-5, as.Date(e[["end"]]), by="day")
    
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
    obs <- getWeatherObsForTournament(event)
    retinfo <- data.frame(event[["course.1"]])
    obs$date <- as.Date(obs$date, format="%Y%m%d")
    
    
    
    weather_sum <- obs %>% group_by(date) %>% dplyr::summarise(mean_temp = mean(tempF, na.rm=TRUE), precip = sum(precip, na.rm=TRUE), mean_wind = mean(windSpeed, na.rm=TRUE), max_wind = max(windSpeed, na.rm=TRUE), max_gust = max(windGust, na.rm=TRUE), wind_direction_variance = circ.disp(windDirDeg * pi/180)[["var"]])
    #print(weather_sum$date)
    print(event[["start"]])
    weather_sum$tourn_day <- weather_sum$date - as.Date(event[["start"]])
    
    melted <- melt(weather_sum ,id.vars=c("tourn_day"))
    melted$col <- paste("day", as.character(melted$tourn_day),as.character(melted$variable), sep="_")
    
    transposed <- as.data.frame(t(melted[,"value"]))
    colnames(transposed) <- t(melted[,"col"])
    transposed$course <- event[["course.1"]]
    transposed$event <- event[["tourn"]]
    return(transposed)
}


tournamentWeatherSummaries <- function(evnts){
    sums <- apply(evnts, 1, tournamentWeatherSummary)
    binded <- do.call("rbind.fill", sums)
    
    #convert dates back, yeah this is a hack
    binded$`day_-3_date` <- as.Date(binded$`day_-3_date`, origin="1970-01-01")
    binded$`day_-2_date` <- as.Date(binded$`day_-2_date`, origin="1970-01-01")
    binded$`day_-1_date` <- as.Date(binded$`day_-1_date`, origin="1970-01-01")
    binded$`day_0_date` <- as.Date(binded$`day_0_date`, origin="1970-01-01")
    binded$`day_1_date` <- as.Date(binded$`day_1_date`, origin="1970-01-01")
    binded$`day_2_date` <- as.Date(binded$`day_2_date`, origin="1970-01-01")
    binded$`day_3_date` <- as.Date(binded$`day_3_date`, origin="1970-01-01")
    
    # move columns to front
    col_idx <- grep("course", names(binded))
    binded <- binded[, c(col_idx, (1:ncol(binded))[-col_idx])]
    col_idx <- grep("event", names(binded))
    binded <- binded[, c(col_idx, (1:ncol(binded))[-col_idx])]
    
    return(binded)
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
        print("Sleeping for 1 minute")
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
    wugKey <-"61b573b303c14284"
    wugDateStr <- getWugDateFormat(dateStr)
    addr <- paste(course[["hole_lat"]], course[["hole_lon"]], sep=",") 
    
    if(length(addr) == 0){
        stop("No coordinates for course")
    }
    
    filename <- paste0("./data/weather/",addr, "-", wugDateStr, ".json")
    filename <- gsub(" ", "", filename)
    
    if(file.exists(filename)){
        print(paste("getting", filename, "locally"))
        weatherContent <- read_file(filename)
    }else{
        #no weather locally, grab file and save it
        
        wugUrl <- paste("http://api.wunderground.com/api/", wugKey,"/history_",wugDateStr, "/q/", addr, ".json", sep = "")
        wugUrl <- gsub(" ", "", wugUrl)
        print(paste("getting weather info ", wugUrl))
        weatherReq <- GET(wugUrl)
        weatherContent <- content(weatherReq, as="text")
        print(paste("saving weather to file", filename))
        write_file(weatherContent, filename)
    }
    
    
    return(weatherContent)
}

dedupe_precip <- function(precip, isMetar){
    
    # precip measurements is calculated in amount since last METAR,  but we have SPECI measurments as well
    # so this function removes the amount already accounted for from last METAR from current measurement
    
    if(is.na(precip) | precip < -999){
        # null value dont bother
        return(precip)   
    }
    
    # remove sum since last metar
    changed_val <- precip - sum_so_far 
    
    if(!isMetar){
        # not a metar reading, subtract what weve taken
        # editing global variable
        sum_so_far <<- sum_so_far + changed_val
    }else{
        # editing global variable
        sum_so_far <<- 0
    }
    return(changed_val)
}




getObsFromWeatherResp <- function(weatherContent, for_mark = FALSE){
    
    # for mark is whether to convert 99's to NAS
    
    
    #from a json response get the observations
    
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    observations <- weatherJSON$history$observations

    
    #remove nested dataframe and add info in separate columns
    hr <- observations$date$hour
    observations$hour <- hr
    min  <- observations$date$min
    time <- as.integer(paste(hr, min, sep=""))
    time <- unlist(lapply(time, fixTime))
    tz <- observations$date$tzname[[1]]
    
    year <- observations$date$year
    month <- observations$date$mon
    day <- observations$date$mday
    date <- paste(year, month, day, sep="")
    
    if(weatherJSON$history$date$tzname !="America/New_York"){
        print(paste("TIMEZONE NOT STANDARD", tz))
    }
    
    date_time <- as.POSIXct(paste(date, time), format = "%Y%m%d %H:%M", tz = tz)
    
    observations$date <- date
    observations$time <- time
    observations$date_time <- date_time
   

    
    #drop utc nested dataframe, other column which is probably meter name, 
    observations <- observations[, !(colnames(observations) %in% c("utcdate" ))]
    
    dataWeWant <- c("tempi", "hum", "wdird","wdire", "wgusti", "wspdi","precipi","rain", "conds", "time", "date", "date_time", "hour", "metar")
    observations <- observations[,dataWeWant]
    colnames(observations) <- c("tempF", "humidity", "windDirDeg","windDirOrd", "windGust","windSpeed" ,"precip","rain", "conds", "weatherTime", "date", "date_time", "hour", "metar")
    
   
    
    observations$tempF <- as.numeric(observations$tempF)
    observations$windDirDeg <- as.numeric(observations$windDirDeg)
    observations$windSpeed <- as.numeric(observations$windSpeed)
    observations$precip <- as.numeric(observations$precip)
    observations$metar <- strtrim(observations$metar, 11)
    
    # de dupe precip values
    is_metar <- grepl("METAR", observations$metar)
    
    # need this global var in dedupe_precip
    sum_so_far <<- 0
    observations$precip <- unlist(mapply(dedupe_precip, observations$precip, is_metar))
    
    if(!for_mark){
        # keep -9999 values. need to adjust the mean calculations in this case
        observations$precip <- unlist(lapply(observations$precip, fix99))
        observations$windGust <- unlist(lapply(observations$windGust, fix99))
        observations$windSpeed <- unlist(lapply(observations$windSpeed, fix99))
        
    }
    
    # make precip since last ob, not always hourly
    prev_date_time <- c(observations$date_time[1] - 3600, observations$date_time[1:length(observations$date_time)-1])
    observations$time_since_prev_ob <- observations$date_time - prev_date_time
    
    #observations$precip <- fixPrecip(observations$preci)
    
    return(observations)
}




getWeatherObsForTournament <- function(tournament, for_mark = FALSE){
    dates <- seq.Date(as.Date(tournament[["start"]])-3, as.Date(tournament[["end"]]), by="day")
    responses <- lapply(dates, getWeatherResponseForCourseDate, tournament)
    obs_list <- lapply(responses, getObsFromWeatherResp, for_mark)
    observation_frame <- do.call("rbind", obs_list)
    return(observation_frame)
}


getHourlyWeather <- function(obs){
    obs$date <- as.Date(obs$date_time)
    precip_by_hr <- obs %>% group_by(hour, date) %>% summarise(rain = sum(precip), mean_wind = mean(windSpeed))
    dtstr <- paste0(precip_by_hr$date, " ", precip_by_hr$hour, ":00")
    #print(dtstr)
    precip_by_hr$datetime <-  as.POSIXct(dtstr, format = "%Y-%m-%d %H:%M")
    #print(precip_by_hr$datetime)
    precip_by_hr <- precip_by_hr[order(precip_by_hr$date),]
    return(precip_by_hr)
}





