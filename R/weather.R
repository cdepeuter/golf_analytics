#' Scrape weather info for all dates in a tournament
#'
#' This grabs the weather response for 3 days before up to the end of a tournament
#' @param event from getPGAEvents() function
#' @return nothing
#' @export
#' @examples
#' scrapeWeatherForTournament(masters09)

scrapeWeatherForTournament <- function(e){
    # get weather for 3 days before tournament
    dates <- seq.Date(as.Date(e[["start"]])-5, as.Date(e[["end"]]), by="day") %>% lapply(getWugDateFormat)
    locString <- paste(e[, c("hole_lat", "hole_lon")], collapse = ",")
    reqs <- paste(locString, dates, sep="-")

    #print(coords)
    t <- lapply(reqs, makeWeatherRequest)
    return(t)
}


#' Scrape weather info for all tournaments
#'
#' This grabs the weather response for 3 days before up to the end of multiple tournaments
#' respecting the rate limit of the weather underground api
#' @param event from getPGAEvents() function
#' @return nothing
#' @export
#' @examples
#' scrapeWeatherForTournaments(events)
scrapeWeatherForTournaments <- function(events){
    #scrape weather, sleep so limit is not reached
    apply(events, 1, function(event){
        scrapeWeatherForTournament(event)
        Sys.sleep(60)
    })
    return()
}


tournamentWeatherSummary <- function(event){
    # TODO FIX THSI
    queries <- paste(event[,c("hole_lat", "hole_lon")], event[,c("start", "end")])
    obs <- getWeatherObsLocationDates(queries)
    retinfo <- data.frame(event[["course.1"]])
    obs$date <- as.Date(obs$date, format="%Y%m%d")
    
    
    
    weather_sum <- obs %>% group_by(date) %>% dplyr::summarise(mean_temp = mean(tempF, na.rm=TRUE), precip = sum(precip, na.rm=TRUE), mean_wind = mean(windSpeed, na.rm=TRUE), max_wind = max(windSpeed, na.rm=TRUE), max_gust = max(windGust, na.rm=TRUE), wind_direction_variance = circ.disp(windDirDeg * pi/180)[["var"]])
    #print(weather_sum$date)
    #print(event[["start"]])
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


#' On a given day/course get the weather observations from a local file
#'
#' This function loads the weather json from a local directory and writes summary information for that day
#' @param dateString date of obs
#' @param coords hase hole_lat, hole_lon
#' @return summary info for event
#' @export
#' @examples
#' makeWeatherRequest( coords, "20150203")


makeWeatherRequest <- function(query){
    # get weather info json response for event at given address on date
    # input: course info with city, state, zip(maybe)
    # output json response from weather underground api
    
    # DONT run this function in vectorized format on an array
    # weatherUnderground maxes API calls at 10 per minute
    
    #get weather and addr for filename
    wugKey <-"61b573b303c14284"
    
    #print(query)
    filename <- paste0("./data/weather/", query, ".json")
    
    if(file.exists(filename)){
        print(paste("getting", filename, "locally"))
        weatherContent <- read_file(filename)
    }else{
        #no weather locally, grab file and save it
        query <- paste0(substr(query, nchar(query)-7, nchar(query)), '/q/', substr(query, 0, nchar(query)-8))
        #print(query)
        wugUrl <- paste("http://api.wunderground.com/api/", wugKey,"/history_", query, ".json", sep = "")
        wugUrl <- gsub(" ", "", wugUrl)
        print(paste("getting weather info from API, saving to local", wugUrl))
        weatherReq <- GET(wugUrl)
        weatherContent <- content(weatherReq, as="text")
        write_file(weatherContent, filename)
        
        # count requests, save time 
        numWeatherRequests <<- numWeatherRequests + 1
        lastWeatherRequest <- Sys.time()
    }
    
    if(exists("numWeatherRequests") & (numWeatherRequests > 9)){
        # has it been a minut eince last request?
        if(difftime(lastWeatherRequest , Sys.time() , "mins") < -1){
            #reset count, been over a minute since last request
            numWeatherRequests <<- 0
        }else{
            #its been less than an minute and 10 requests
            print("10 requests recorded in last minute, sleeping")
            Sys.sleep(60)
            numWeatherRequests <<- 0
        }
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




getObsFromWeatherResp <- function(weatherContent){
    
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
    
    # if(weatherJSON$history$date$tzname !="America/New_York"){
    #     print(paste("TIMEZONE NOT STANDARD", tz))
    # }
    
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
    
  
    # turn -9999s into 0
    observations$precip <- unlist(lapply(observations$precip, fix99))
    observations$windGust <- unlist(lapply(observations$windGust, fix99))
    observations$windSpeed <- unlist(lapply(observations$windSpeed, fix99))
    
    # make precip since last ob, not always hourly
    prev_date_time <- c(observations$date_time[1] - 3600, observations$date_time[1:length(observations$date_time)-1])
    observations$time_since_prev_ob <- observations$date_time - prev_date_time
    
    #observations$precip <- fixPrecip(observations$preci)
    
    return(observations)
}


fix99 <- function(data){
    if(is.na(data) | data == "-9999.00" | data == "-9999.0" | data == -9999.0 ){
        return(0)
    }
    return(data)
}

#' Get Weather for location and dates
#' 
#' For an event load all weather data for that tournament into a data frame
#' @param list queries list of queries to grab observations from in "lat,lon-date"
#' @return dataframe of weather observations 
#' @export
#' @import geosphere
#' @examples
#' getWeatherObsLocationDates(queries)

getWeatherObsLocationDates <- function(queries){
    ## main function used for loading weather info for a tournament
   
    responses <- lapply(queries, makeWeatherRequest)
    obs_list <- lapply(responses, getObsFromWeatherResp)
    observation_frame <- do.call("rbind", obs_list)
    
    #get location for weather station
    weather_station_code <- strsplit(observation_frame$metar[1], split = " ")[[1]][[2]]
    weather_station_coords <- getStationLocation(weather_station_code)

    # distance from course to station
    course_coords <- as.double(c(loc[["hole_lon"]], loc[["hole_lat"]]))
    
    dist <- distVincentySphere(course_coords, as.double(weather_station_coords)) * 0.000621371 
    
    # add to frame
    observation_frame$dist_from_weather_miles <- dist
    
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


#' For a given station code, return the coordinates of that station
#'
#' @param String airport_code
#' @return c("lat","lon")
#' @export
#' @examples
#' scrapeWeatherForTournament(masters09)

getStationLocation <- function(station_code){
    wugKey <- "61b573b303c14284"
    station_lookup_url <- paste0("http://api.wunderground.com/api/", wugKey,"/geolookup/q/",station_code,".json")
    station_resp <- getUrlResponse(station_lookup_url)
    
    station_json <- jsonlite::fromJSON(station_resp)
    candidate_stations <- station_json$location$nearby_weather_station$airport$station
    station_coords <- candidate_stations[which(candidate_stations$icao == station_code)[1], c("lon", "lat")]
    
    return(as.double(station_coords))
}





