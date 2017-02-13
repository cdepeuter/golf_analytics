
getShotlinkTable <- function(filename){
    tb <- read.csv(paste0("./data/shotlink/", filename),  sep=";", header=TRUE, stringsAsFactors = FALSE)
    return(tb)
}

getWetnessForShot <- function(shot){
    observations <- getObservationsBeforeShot()
}



getObservationsBeforeShot <- function(shot, weather){
    
    
}


getWeatherObsForTournament <- function(tournament){
    dates <- seq.Date(as.Date(tournament[["start"]])-3, as.Date(tournament[["end"]]), by="day")
    responses <- lapply(dates, getWeatherResponseForCourseDate, tournament)
    obs_list <- lapply(responses, getObsFromWeatherResp)
    observation_frame <- do.call("rbind", obs_list)
    return(observation_frame)
}



fix99 <- function(data){
    if(data == "-9999.00"){
        return(0)
    }
    return(as.double(data))
}


getWeatherBeforeShot <- function(shot, observations){
    # for a given shot, find the weather observation with the closest time
    # assuming the observations are at the right zip code and date
    # input: shot in shotlink format, observations in weatherUnderground format
    # output: rain, wind data
    
    
    whichObs <- which.min((abs(observations[["time"]] - as.integer(shot[["Time"]]) + 5000 * (as.Date(shot[["Date"]], format="%m/%d/%Y") - as.Date(observations[["date"]], format="%Y%m%d")))))

    closestObservation <- observations[1:whichObs,] 
    
    dataWeWant <- c("tempi", "hum", "wdird","wdire", "wgusti","precipi","rain", "conds", "time", "date")
    dat <- closestObservation[,dataWeWant]
    colnames(dat) <- c("tempF", "humidity", "wDirDeg","wDir", "windGust","precip","rain", "conds", "weatherTime", "date")
    dat$precip <- lapply(dat$precip, fix99)
    return(dat)
}


getObsFromWeatherResp <- function(weatherContent){
    #from a json response get the observations
    
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
