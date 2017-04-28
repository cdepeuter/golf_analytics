#' This function returns a data frame row of summary weather information for a shot
#' @param date_time shot_date_time input first year of events to return, Defaults to 0 (no lower bound)
#' @param data.frame weather last year of events to return, Defaults to 10000 (no upper bound)
#' @param boolean for_mark format data for mark or no
#' @keywords pga golf
#' @return Data Frame row of summary weather info for the shot
#' @export
#' @examples shotWeatherSummary(safeway.shots[1,], safeway.weather)
#' @import httr
#' @import dplyr
#' 

shotWeatherSummary <- function(shot_date_time, weather, for_mark = FALSE){
    # precip x hrs before shot
    max_hrs_back <- 48
    hours_to_look <- c(1, 2, 4, 6, 8, 12, 18, 24, 36, 48)
    

    weather_time_diff <- weather$date_time - shot_date_time
    units(weather_time_diff) <- "mins"

    relevant_weather <- weather[which(weather_time_diff > - 6 * 60 & weather_time_diff <= 0),]
    
    
    rain_before_shot <- lapply(hours_to_look, function(x){
        return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0), "precip"], na.rm = TRUE))
    })
    rain_df <- data.frame(as.list(rain_before_shot))
    
    # proportionally include first observation past response
    first_obs_after <- head(weather[which( weather_time_diff > 0),], n=1)
    last_obs_before <- tail(relevant_weather, n=1)
  
    
    # get minute difference between obs before and obs after
    time_between_obs <- first_obs_after[, "date_time"] - last_obs_before[, "date_time"]
    time_between_obs_shot <- time_between_obs - (first_obs_after[, "date_time"] - shot_date_time)
    units(time_between_obs) <- "mins"
    units(time_between_obs_shot) <- "mins"
    prop_rain_after <- first_obs_after[,"precip"] * (as.integer(time_between_obs_shot))/as.integer(time_between_obs)

    
    #print(colnames(last_obs_before))
    #cumulative_rain_cols <-  paste0("rain_",  hours_to_look, "_hrs_before")
    #print(cumulative_rain_cols)
   
    # 
    # # weather dataframe will look different depending on if this is called individually or through matchShotsToWeather
    # if(!all(cumulative_rain_cols %in% colnames(weather))){
    #     rain_before_shot <- lapply(hours_to_look, function(x){
    #         return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0), "precip"]))
    #     })
    #     
    #     rain_df <- data.frame(as.list(rain_before_shot))
    # }else{
    #     rain_df <- last_obs_before[, cumulative_rain_cols]
    # }
   
    
    # proportionally add rain after last obs
    rain_df <- rain_df + round(prop_rain_after, 4)
    colnames(rain_df) <-  paste0("rain_",  hours_to_look, "_hrs_before")
    
    mins_since_obs <- difftime(shot_date_time, last_obs_before$date_time, units = "mins")
    last_wind_speed <- last_obs_before$windSpeed
    last_wind_gust <- last_obs_before$windGust
    last_wind_dir <- last_obs_before$windDirOrd
    
    
    last_wind_dir_degrees <- last_obs_before$windDirDeg
    latest_weather_wind <- weather[which(weather_time_diff > - 2 * 60 & weather_time_diff <= 0), "windSpeed"]
    mean_wind_2hrs_before <- mean(latest_weather_wind, na.rm = TRUE)
    
    weather_time <- last_obs_before$date_time
    

    return(cbind(data.frame( mins_since_obs, last_wind_speed, last_wind_gust, last_wind_dir_degrees, last_wind_dir, mean_wind_2hrs_before), rain_df))
}

#' format_for_mark
#'
#' This function takes relevant columns from a shot_weather dataframe and turns it into a regular data frame
#' @param data.frame shot_weather weather and shots for tournament
#' @param boolean for_mark format data for mark or no
#' @keywords pga golf
#' @return Data Frame row of just weather infor for each shot
#' @export
#' @examples format_for_mark(all.shot.weather)
#' 


format_for_mark <- function(shot_weather){
    # use this to add identification columns to files before giving to mark
    
    # replace nas
    shot_weather[is.na(shot_weather)] <- -9999
    
    shot_cols <- c("season", "course", "perm_tourn", "round" ,"hole", "shot_num", "player", "shot_degrees", "target_degrees", "wind_target_angle_diff",
                   "mins_since_obs", "dist_from_weather_miles", "last_wind_speed", "last_wind_gust","last_wind_dir_degrees", "last_wind_dir", "mean_wind_2hrs_before", 
                   "rain_0_to_1_hrs_before" ,  "rain_1_to_2_hrs_before" ,  "rain_2_to_4_hrs_before" ,  "rain_4_to_6_hrs_before" , 
                    "rain_6_to_8_hrs_before"  , "rain_8_to_12_hrs_before",  "rain_12_to_18_hrs_before", "rain_18_to_24_hrs_before",
                     "rain_24_to_36_hrs_before", "rain_36_to_48_hrs_before")
    
    shot_weather <- shot_weather[,shot_cols]
    return(shot_weather)
}

#' #' get_cumulative_rain
#' #'
#' #' This function turns the rain observations which are overlapping 
#' #' @param date_time shot_date_time input first year of events to return, Defaults to 0 (no lower bound)
#' #' @param data.frame weather last year of events to return, Defaults to 10000 (no upper bound)
#' #' @param boolean for_mark format data for mark or no
#' #' @keywords pga golf
#' #' @return Data Frame row of summary weather info for the shot
#' #' @export
#' #' @examples shotWeatherSummary(safeway.shots[1,], safeway.weather)
#' #' @import httr
#' #' @import dplyr
#' #' 
#' 
#' 
#' 
#' get_cumulative_rain <- function(obs_time, weather){
#'     hours_to_look <- c(1, 2, 4, 6, 12, 18, 24, 36, 48)
#'     
#'     weather_time_diff <- weather$date_time - obs_time
#'     units(weather_time_diff) <- "mins"
#'     
#'     
#'     rain_before_obs <- lapply(hours_to_look, function(x){
#'         return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0), "precip"], na.rm = TRUE))
#'     })
#'     
#'     
#'     rain_df <- data.frame(as.list(rain_before_obs))
#'     colnames(rain_df) <-  paste0("rain_",  hours_to_look, "_hrs_before")
#'     
#'     return(rain_df)
#' }


#' matchWeatherToShots
#'
#' This function takes all shots in a tournament, and all weather observations we have for the tournament
#' and returns a dataframe with relevant weather info for each shot
#' @param shots data.frame input first year of events to return, Defaults to 0 (no lower bound)
#' @param data.frame weather all weather obs for the tournament
#' @return Data Frame  summary weather info for each shot
#' @export
#' @examples matchWeatherToShots(safeway.shots, safeway.weather)


matchWeatherToShots <- function(shots, weather){
    
    #get cumulative rain and bind to weather ( so you dont need to do this for every shot)
    #
    # weather.cumulative_rain <- lapply(weather$date_time, get_cumulative_rain, weather)
    # weather.cumulative_rain.binded <- do.call("rbind", weather.cumulative_rain)
    # weather <- cbind(weather, weather.cumulative_rain.binded)
    
    summaries <- lapply(shots$date_time, shotWeatherSummary, weather)
    
    weatherInfo <- bind_rows(summaries)
    
    weatherInfo$wind_target_angle_diff <- getWindShotDiff(cbind(shots, weatherInfo))
    weatherInfo$dist_from_weather_miles <- weather$dist_from_weather_miles[1]
    
    #“rain_1_hrs_before";"rain_1_to_2_hrs_before";"rain_2_to_4_hrs_before";"rain_4_to_6_hrs_before";"rain_6_to_12_hrs_before";"rain_12_to_18_hrs_before";"rain_18_to_24_hrs_before";"rain_24_to_36_hrs_before";"rain_36_to_48_hrs_before"
    
    # make weather non-overlapping
    weatherInfo$rain_0_to_1_hrs_before <- weatherInfo$rain_1_hrs_before 
    weatherInfo$rain_1_to_2_hrs_before <- weatherInfo$rain_2_hrs_before -  weatherInfo$rain_1_hrs_before
    weatherInfo$rain_2_to_4_hrs_before <- weatherInfo$rain_4_hrs_before -  weatherInfo$rain_2_hrs_before
    weatherInfo$rain_4_to_6_hrs_before <- weatherInfo$rain_6_hrs_before -  weatherInfo$rain_4_hrs_before
    weatherInfo$rain_6_to_8_hrs_before <- weatherInfo$rain_8_hrs_before -  weatherInfo$rain_6_hrs_before
    weatherInfo$rain_8_to_12_hrs_before <- weatherInfo$rain_12_hrs_before -  weatherInfo$rain_8_hrs_before
    weatherInfo$rain_12_to_18_hrs_before <- weatherInfo$rain_18_hrs_before -  weatherInfo$rain_12_hrs_before
    weatherInfo$rain_18_to_24_hrs_before <- weatherInfo$rain_24_hrs_before -  weatherInfo$rain_18_hrs_before
    weatherInfo$rain_24_to_36_hrs_before <- weatherInfo$rain_36_hrs_before -  weatherInfo$rain_24_hrs_before
    weatherInfo$rain_36_to_48_hrs_before <- weatherInfo$rain_48_hrs_before -  weatherInfo$rain_36_hrs_before
    
    weatherInfo$agg_48_hr_rain <- aggRain(weatherInfo)
    old_weather_cols <- c("rain_1_hrs_before","rain_2_hrs_before","rain_4_hrs_before","rain_6_hrs_before","rain_8_hrs_before","rain_12_hrs_before",
                          "rain_18_hrs_before","rain_24_hrs_before","rain_36_hrs_before","rain_48_hrs_before")
    
    
    weatherInfo <- weatherInfo[,!colnames(weatherInfo) %in% old_weather_cols]
    
    # put it all together
    weatherInfo <- cbind(shots, weatherInfo)
    return(weatherInfo)
}

#' aggRain
#'
#' This function returns combines all rain columns into an aggregate over all time
#' @param weatherInfo weather columns from a dataframe
#' @return Column of aggregate rain



aggRain <- function(weatherInfo){
   agg <- weatherInfo$rain_0_to_1_hrs_before + weatherInfo$rain_1_to_2_hrs_before + weatherInfo$rain_2_to_4_hrs_before +
        weatherInfo$rain_4_to_6_hrs_before + weatherInfo$rain_6_to_8_hrs_before + weatherInfo$rain_8_to_12_hrs_before +
        weatherInfo$rain_12_to_18_hrs_before +  weatherInfo$rain_18_to_24_hrs_before + weatherInfo$rain_24_to_36_hrs_before + weatherInfo$rain_36_to_48_hrs_before

    return(agg)
}
