
getShotlinkTable <- function(filename){
    tb <- read.csv(paste0("./data/shotlink/", filename),  sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    # remmove last column and row
    tb <- tb[, colSums(is.na(tb)) < nrow(tb)]
    
    time <- unlist(lapply(tb$Time, fixTime))
    tb$time <- time
    date <- tb$Date
    date_time <- as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M")
    
    # TODO not standardied
    tb$hour <- hours(date_time)-4
    tb$date_time <- date_time
    
    return(tb)
}


getShotlinkExtTable <- function(filename, timezone = " "){
    tb <- read.csv(paste0("./data/shotlink/", filename),  sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    # remmove last column and row
    tb <- tb[, colSums(is.na(tb)) < nrow(tb)]
    tb <- tb[1:nrow(tb)-1,]
    
    time <- unlist(lapply(tb$time, fixTime))
    tb$time <- time
    
    date <- paste(tb$act_month, tb$act_day, tb$act_year, sep="/")
    tb$Date <- as.Date(date, format = "%m/%d/%Y")
    
    
    date_time <- as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M", tz=timezone)
    
    # TODO not standardied
    tb$hour <- hours(date_time)-4
    tb$date_time <- date_time
    
    tb$shot_degrees <-  unlist(apply(tb, 1, getShotDegrees))
    tb$aim_degrees <- unlist(apply(tb, 1, getAimDegrees))
    
    return(tb)
}
# 
# getWindScore <- function(obs){
#     meanWind
#     var <- var(obs$)
# }

getLocalTZ <- function(event){
    googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'
    url <- paste0("https://maps.googleapis.com/maps/api/timezone/json?location=", gsub(" ","", paste(event[["hole_lat"]], event[["hole_lon"]], sep=",")), "&timestamp=1&key=", googleKey)
    print(url)
    resp <- jsonlite::fromJSON(getUrlResponse(url))
    return(resp$timeZoneId)
}

getHourlyWeather <- function(obs){
    precip_by_hr <- obs %>% group_by(hour, date) %>% summarise(rain = sum(precip), mean_wind = mean(windSpeed))
    precip_by_hr$datetime <-  as.POSIXct(paste0(precip_by_hr$date, " ", precip_by_hr$hour, ":00"), format = "%Y%m%d %H:%M")
    precip_by_hr <- precip_by_hr[order(precip_by_hr$date),]
    return(precip_by_hr)
}

plotRainAndDrives <- function(shots, obs, plotTitle = ""){
    precip_by_hr <- getHourlyWeather(obs)
    
    drives <- shots[shots$shot_num == 1 & shots$par > 3 & shots$loc_end == 4,]
    hourly_drive_distance <- drives %>% group_by(hour, Date) %>% summarise(dist = mean(shot_dis..inch.))
    hourly_drive_distance$datetime <-  as.POSIXct(paste0(hourly_drive_distance$Date," ", hourly_drive_distance$hour, ":00"), format = "%Y-%m-%d %H:%M")
    hourly_drive_distance$dist_norm <- hourly_drive_distance$dist / max(hourly_drive_distance$dist)
    hourly_drive_distance <- hourly_drive_distance[order(hourly_drive_distance$Date),]
    
    plot(precip_by_hr$datetime, precip_by_hr$rain, main=paste("Rain and Drive Dist", plotTitle), xlab = "time", ylab="Precip total and drive dist", ylim = c(-.01,1) ) 
    lines(precip_by_hr$datetime, precip_by_hr$rain)
    points(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm)
    lines(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm, col=2)
}



plotWindAndDrives <- function(shots, obs, plotTitle = ""){
    precip_by_hr <- getHourlyWeather(obs)
    
    drives <- shots[shots$shot_num == 1 & shots$par > 3 & shots$loc_end == 4,]
    hourly_drive_distance <- drives %>% group_by(hour, Date) %>% summarise(dist = mean(shot_dis..inch.))
    hourly_drive_distance$datetime <-  as.POSIXct(paste0(hourly_drive_distance$Date," ", hourly_drive_distance$hour, ":00"), format = "%Y-%m-%d %H:%M")
    hourly_drive_distance$dist_norm <- hourly_drive_distance$dist / max(hourly_drive_distance$dist)
    hourly_drive_distance <- hourly_drive_distance[order(hourly_drive_distance$Date),]
    
    plot(precip_by_hr$datetime, precip_by_hr$mean_wind / max(precip_by_hr$mean_wind), main=paste("Wind and Drive Dist", plotTitle), xlab = "time", ylab="Mean Wind and drive dist",  ylim = c(-.01, 1)) 
    lines(precip_by_hr$datetime, precip_by_hr$mean_wind/max(precip_by_hr$mean_wind))
    points(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm)
    lines(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm, col=2)
}

shotWeatherSummary <- function(shot_date_time, weather){
    # precip x hrs before shot
    max_hrs_back <- 48
    hours_to_look <- c(2, 4, 6, 12, 18, 24, 36, 48)
    
    #print(typeof(shot_date_time))
    #print( attr(as.POSIXlt( shot[["date_time"]]),"tzone"))
    #if(attr(as.POSIXlt(weather$date_time),"tzone")[1] != attr(as.POSIXlt( shot[["date_time"]]),"tzone")[1]){
    #    warning("weather and shot in different timezones")
    #}
    
    weather_time_diff <- weather$date_time - shot_date_time
    units(weather_time_diff) <- "mins"
    #print(weather_time_diff)
    relevant_weather <- weather[which(weather_time_diff > - max_hrs_back * 60 & weather_time_diff <= 0),]
    
    rain_before_shot <- lapply(hours_to_look, function(x){
        return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0),"precip"], na.rm = TRUE))
    })
    
    rain_df <- data.frame(rain_before_shot)
    colnames(rain_df) <-  paste0("rain_",  hours_to_look, "_hrs_before")
    
    #print(rain_df)
    
    #print(weather_time_diff)
    latest_weather <- weather[which(weather_time_diff > - 2 * 60 & weather_time_diff <= 0),]

    lastObs <- tail(latest_weather, n=1)
    
    #print(lastObs)
    mins_since_obs <- difftime(shot_date_time, lastObs$date_time, units = "mins")
    last_wind_speed <- as.integer(lastObs$windSpeed)
    last_wind_gust <- as.integer(lastObs$windGust)
    last_wind_dir <- lastObs$windDirOrd
    last_wind_dir_degrees <- as.numeric(lastObs$windDirDeg)
    mean_wind_2hrs_before <- mean(latest_weather$windSpeed, na.rm = TRUE)
    weather_time <- lastObs$date_time
    # shot directtion
    # wind last obs before shot
    
    #
    return(cbind(data.frame( mins_since_obs, last_wind_speed, last_wind_gust, last_wind_dir_degrees, last_wind_dir, mean_wind_2hrs_before), rain_df))
}

getShotDegrees <- function(shot){
    
    deltax <- as.numeric(shot[["end_x..feet."]]) - as.numeric(shot[["start_x"]])
    deltay <- as.numeric(shot[["end_y..feet."]]) - as.numeric(shot[["start_y"]])
    
    
    return(getAngle(deltax, deltay))
}

getAimDegrees <- function(shot){
    if(shot[["shot_num"]] == 1){
        deltax <- as.numeric(shot[["med_x"]]) - as.numeric(shot[["start_x"]])
        deltay <- as.numeric(shot[["med_y"]]) - as.numeric(shot[["start_y"]])
    }else{
        deltax <- as.numeric(shot[["hole_x"]]) - as.numeric(shot[["start_x"]])
        deltay <- as.numeric(shot[["hole_y"]]) - as.numeric(shot[["start_y"]])
    }
    
    
    return(getAngle(deltax, deltay))
}


getAngle <- function(delta_x, delta_y){
    #print(delta_x)
    #print(delta_y)
    
    
    val = 90 - atan2(delta_y, delta_x) * 180/pi
    
    #print( atan2(delta_y, delta_x)* 180/pi)
    
    if(val < 0){
        val = 360 + val
    }
    return(val)
}



getWeatherObsForTournament <- function(tournament){
    dates <- seq.Date(as.Date(tournament[["start"]])-3, as.Date(tournament[["end"]]), by="day")
    responses <- lapply(dates, getWeatherResponseForCourseDate, tournament)
    obs_list <- lapply(responses, getObsFromWeatherResp)
    observation_frame <- do.call("rbind", obs_list)
    return(observation_frame)
}



fix99 <- function(data){
    if(data == "-9999.00" | data == "-9999.0"){
        return(NA_integer_)
    }
    return(as.double(data))
}

fixTime <- function(tm){
    if(nchar(tm) == 2){
        return(paste0("00:", tm))
    }
    if(nchar(tm) == 3){
        return(paste0(substr(tm, 1, 1), ":", substr(tm, 2, 3)))
    }
    return(paste0(substr(tm, 1, 2), ":", substr(tm, 3, 4)))
}


getWeatherBeforeShot <- function(shot, observations){
    # for a given shot, find the weather observation with the closest time
    # assuming the observations are at the right zip code and date
    # input: shot in shotlink format, observations in weatherUnderground format
    # output: rain, wind data
    
    
    whichObs <- which.min(abs(as.numeric(observations[["date_time"]] - shot[["date_time"]])))
    closestObservation <- observations[1:whichObs,] 
    
    return(closestObservation)
}


getObsFromWeatherResp <- function(weatherContent){
    #from a json response get the observations
    
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    observations <- weatherJSON$history$observations
    
    # TODO CHECK TIMEZONE DATA
    # maybe use UTC if this is an issue

    
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
    
    dataWeWant <- c("tempi", "hum", "wdird","wdire", "wgusti", "wspdi","precipi","rain", "conds", "time", "date", "date_time", "hour")
    observations <- observations[,dataWeWant]
    colnames(observations) <- c("tempF", "humidity", "windDirDeg","windDirOrd", "windGust","windSpeed" ,"precip","rain", "conds", "weatherTime", "date", "date_time", "hour")
    
    observations$windDireDeg <- as.numeric(observations$windDirDeg)
    observations$precip <- unlist(lapply(observations$precip, fix99))
    observations$windGust <- unlist(lapply(observations$windGust, fix99))
    observations$windSpeed <- as.numeric(observations$windSpeed)
    observations$windSpeed <- unlist(lapply(observations$windSpeed, fix99))
    
    
    return(observations)
}




matchWeatherToShots <- function(shots, weather){
    weatherInfo <- do.call("rbind.data.frame", lapply(shots$date_time, shotWeatherSummary, weather))
    
    return(weatherInfo)
}

bindWeatherToShot <- function(shots, shot_weather){
    shot_cols <- c("season", "course", "perm_tourn", "round" ,"hole", "shot_num", "player", "shot_degrees", "aim_degrees")
    shot_info <- shots[,shot_cols]
    return(cbind(shot_info, shot_weather))
}



getWindShotDiff <- function(shots_n_weather){
    deg_wind_diff <- shots_n_weather$aim_degrees - shots_n_weather$last_wind_dir_degrees
    
    deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
    deg_wind_diff <- deg_wind_diff - 180 # flip the difference
    deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
    
    return(round(deg_wind_diff/2, digits = -1)*2)
}

getWindVsDist <- function(shots_n_weather){
    
    drives <- shots_n_weather[shots_n_weather$shot_num == 1 & shots_n_weather$par > 3 & shots_n_weather$loc_end == 4  &  (shots_n_weather$round == 3 | shots_n_weather$round == 4)  & shots_n_weather$last_wind_speed > 5,]
    
    return(drives %>% group_by(wind_shot_angle_diff) %>% summarise(drive_dist = mean(shot_dis..inch.), num = n()))
    
}


