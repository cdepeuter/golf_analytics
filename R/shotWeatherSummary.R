
shotWeatherSummary <- function(shot_date_time, weather, for_mark = FALSE){
    # precip x hrs before shot
    max_hrs_back <- 48
    hours_to_look <- c(1, 2, 4, 6, 12, 18, 24, 36, 48)
    

    weather_time_diff <- weather$date_time - shot_date_time
    #print(typeof(weather_time_diff))
    units(weather_time_diff) <- "mins"

    relevant_weather <- weather[which(weather_time_diff > - max_hrs_back * 60 & weather_time_diff <= 0),]
    
   
    rain_before_shot <- lapply(hours_to_look, function(x){
        return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0), "precip"], na.rm = TRUE))
    })
    
    # proportionally include first observation past response
    first_obs_after <- head(weather[which( weather_time_diff > 0),], n=1)
    last_obs_before <- tail(relevant_weather, n=1)
  
    
    # get minute difference between obs before and obs after
    time_between_obs <- first_obs_after[, "date_time"] - last_obs_before[, "date_time"]
    time_between_obs_shot <- time_between_obs - (first_obs_after[, "date_time"] - shot_date_time)
    units(time_between_obs) <- "mins"
    units(time_between_obs_shot) <- "mins"
    prop_rain_after <- first_obs_after[,"precip"] * (as.integer(time_between_obs_shot))/as.integer(time_between_obs)
    
    # proportionally add rain after last obs
 
    rain_df <- data.frame(as.list(rain_before_shot))
    rain_df <- rain_df + round(prop_rain_after, 4)
    colnames(rain_df) <-  paste0("rain_",  hours_to_look, "_hrs_before")
    
 
    latest_weather <- weather[which(weather_time_diff > - 2 * 60 & weather_time_diff <= 0),]
    
    lastObs <- tail(latest_weather, n=1)
    
    mins_since_obs <- difftime(shot_date_time, lastObs$date_time, units = "mins")
    last_wind_speed <- as.integer(lastObs$windSpeed)
    last_wind_gust <- as.integer(lastObs$windGust)
    last_wind_dir <- lastObs$windDirOrd
    
    
    last_wind_dir_degrees <- as.numeric(lastObs$windDirDeg)
    
    # calculate means, but remove NAS
    if(for_mark){
        latest_wind_speed <- latest_weather$windSpeed
        # remove NAS
        latest_wind_speed[latest_wind_speed > -100] <- NA_integer_
        
        mean_wind_2hrs_before <-  mean(latest_wind_speed, na.rm = TRUE)
    }else{
        mean_wind_2hrs_before <- mean(latest_weather$windSpeed, na.rm = TRUE)
    }
    
    
    weather_time <- lastObs$date_time
    
    
    # shot directtion
    # wind last obs before shot
    
    #
    return(cbind(data.frame( mins_since_obs, last_wind_speed, last_wind_gust, last_wind_dir_degrees, last_wind_dir, mean_wind_2hrs_before), rain_df))
}







matchWeatherToShots <- function(shots, weather){
    weatherInfo <- do.call("rbind.data.frame", lapply(shots$date_time, shotWeatherSummary, weather))
    
    return(weatherInfo)
}
