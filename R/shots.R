
# 
# getWindScore <- function(obs){
#     meanWind
#     var <- var(obs$)
# }

getLocalTZ <- function(event){
    googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'
    url <- paste0("https://maps.googleapis.com/maps/api/timezone/json?location=", gsub(" ","", paste(event[["hole_lat"]], event[["hole_lon"]], sep=",")), "&timestamp=1&key=", googleKey)
    #print(url)
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
    print(delta_x)
    print(delta_y)
    
    
    val = 90 - atan2(delta_y, delta_x) * 180/pi
    
    #print( atan2(delta_y, delta_x)* 180/pi)
    
    if(val < 0){
        val = 360 + val
    }
    return(val)
}




fix99 <- function(data){
    if(data == "-9999.00" | data == "-9999.0" | data == -9999.0){
        return(NA_integer_)
    }
    return(data)
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



bindWeatherToShot <- function(shots, shot_weather){
    # use this to add identification columns to files before giving to mark
    
    shot_cols <- c("season", "course", "perm_tourn", "round" ,"hole", "shot_num", "player", "shot_degrees", "aim_degrees")
    shot_info <- shots[,shot_cols]
    return(cbind(shot_info, shot_weather))
}




