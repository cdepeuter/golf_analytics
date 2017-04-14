#' These functions plot weather nformation
#'
#' @export
#' @import chron
#' @import ggplot2
#' @import ggpmisc


plotMeanWindSpeedAndDriveDist <- function(shots, obs, plotTitle = ""){
    # line plot of drive distance and wind speed
    
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

rain_distance_lines <- function(shots, groupFactor=2){
    
    rain_diff_by_hrs <- rain_accumulation_time_distances(shots, groupFactor = groupFactor)
    
    ggplot(rain_diff_by_hrs[rain_diff_by_hrs$obs > 20,]) + geom_line(aes(x=hrs, y=dist_diff, group=rain, colour=rain)) + scale_colour_gradient() +
        geom_text( aes(x=hrs, y=dist_diff, label = rain, colour =rain))
    #return(rain_diff_by_hrs)
}


plot_rain_regressions <- function(shots, isolate_interval){
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    # for regression coefficients we only want to look at long holes
    shots <- shots[shots$long_hole ,]
    
    lapply(rain_cols, function(x){
        shots_ <- shots
        if(isolate_interval){
            shots_ <- shots_[shots_[,"agg_48_hr_rain"] == shots_[,x],]
        }
        shots_ <- shots_[shots_[,x] != 0,] 
        ggplot(shots_, aes_string(x=x, y="drive_dist_diff")) + geom_point()  +
            stat_smooth(method="lm", formula = y ~ x) + stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE, position=position_jitter(width=.5))
        
    })
    
}



plotRainAndDriveDist <- function(shots, obs, plotTitle = ""){
    # line plot of precipitation and drive distance
    
    precip_by_hr <- getHourlyWeather(obs)
    
    hourly_drive_distance <- getHourlyDriveDist(shots)
    
    plot(precip_by_hr$datetime, precip_by_hr$rain, main=paste("Rain and Drive Dist", plotTitle), xlab = "time", ylab="Precip total and drive dist", ylim = c(-.01,1) )
    lines(precip_by_hr$datetime, precip_by_hr$rain)
    points(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm)
    lines(hourly_drive_distance$datetime, hourly_drive_distance$dist_norm, col=2)
    
    #binded_hourly <- merge(precip_by_hr, hourly_drive_distance, by = c("hour", "date"), all=TRUE)
    
    #ggplot(binded_hourly, aes(x=datetime.x)) + geom_line(aes(y=dist_norm), color="red") + geom_line(aes(y=rain))  + geom_point(aes(x=datetime.x, y=rain)) + ggtitle(plotTitle)
}


getWindSpeedVsDist <- function(shots_n_weather, tourney = " "){
    shots_n_weather$wind_target_angle_diff <- getWindShotDiff(shots_n_weather)
    shots_n_weather$opposing <- shots_n_weather$wind_target_angle_diff > 120 & shots_n_weather$wind_target_angle_diff < 240
    
    shots_n_weather <- shots_n_weather[shots_n_weather$opposing, ]
    drives <- shots_n_weather[shots_n_weather$shot_num == 1 & shots_n_weather$par > 3 & shots_n_weather$loc_end == 4,]
    
    speedVDist <- drives %>% dplyr::group_by(last_wind_speed)  %>% dplyr::summarise(drive_dist = mean(shot_dis..inch.), n = n())
    
    # only take where numObs > 10
    speedVDist <- speedVDist[speedVDist$n >= 10,]
    ggplot(speedVDist, aes(x=last_wind_speed)) + geom_line(aes(y=drive_dist)) + ggtitle(tourney)
    
}

windAnalysisForTourney <- function(shot_weather, tourney=""){
    shot_weather$wind_target_angle_diff <- getWindShotDiff(shot_weather)
    wind_vs_dist <- getWindVsDist(shot_weather)
    
    ggplot(wind_vs_dist, aes(x=wind_target_angle_diff)) + geom_line(aes(y=drive_dist)) + ggtitle(tourney)
    
}


plotDriveDistAdjustWeather <- function(player.drives){
    
    title <- paste(player.drives$player_first[1], player.drives$player_last[1], "drives")
   
    
    player.drives$opp_wind <- player.drives$wind_target_angle_diff > 90 & player.drives$wind_target_angle_diff < 270 & player.drives$last_wind_speed > 6
    
    ggplot(player.drives) + geom_point(aes(x=date_time, y=shot_dis..yards., color=opp_wind, size=last_wind_speed)) + ggtitle(title)
}



plotDriveDistAdjustWeatherMultipleTourneys <- function(player.drives){
    
    title <- paste(player.drives$player_first[1], player.drives$player_last[1], "drives, wind and rain adjusted")
    
    player.drives$indx <- seq(1, nrow(player.drives))
    
    player.drives$opp_wind <- player.drives$net_wind < 0
    
    ggplot(player.drives) + geom_point(aes(x=indx, y=adjusted_dist, color=long_hole)) + ggtitle(title) + scale_y_continuous(limits=c(225, 375))
}


plotDriveDistMultipleTourneys <- function(player.drives){
    
    title <- paste(player.drives$player_first[1], player.drives$player_last[1], "drives")
    
    player.drives$indx <- seq(1, nrow(player.drives))
    
    player.drives$opp_wind <- player.drives$net_wind < 0
    
    ggplot(player.drives) + geom_point(aes(x=indx, y=shot_dis..yards., color=opp_wind, size=last_wind_speed)) + ggtitle(title)
}








