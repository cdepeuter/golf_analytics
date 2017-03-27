getWindShotDiff <- function(shots_n_weather){
    deg_wind_diff <- shots_n_weather$target_degrees - shots_n_weather$last_wind_dir_degrees
    
    deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
    deg_wind_diff <- deg_wind_diff - 180 # flip the difference
    deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
    
    return(round(deg_wind_diff, digits = -1))
}





getWindVsDist <- function(shots_n_weather, holes = 1:18, rounds = 1:5){
    
    drives <- shots_n_weather[shots_n_weather$shot_num == 1 & shots_n_weather$par > 3 & shots_n_weather$loc_end == 4  & shots_n_weather$last_wind_speed > 6,]
    drives <- drives[(drives$hole %in% holes)  &  (drives$round %in% rounds),]
    print(dim(drives))
    return(drives %>% dplyr::group_by(wind_target_angle_diff) %>% dplyr::summarise(drive_dist = mean(shot_dis..yards.)) )
    
}

getWindVsDistByHole <- function(shots_n_weather){
    
    shots_n_weather$opposing <- shots_n_weather$wind_target_angle_diff > 120 & shots_n_weather$wind_target_angle_diff < 240
    

    drives <- shots_n_weather[shots_n_weather$shot_num == 1 & shots_n_weather$par > 3 & shots_n_weather$loc_end == 4  & shots_n_weather$last_wind_speed > 6,]
    diff <- drives %>% dplyr::group_by(opposing, hole) %>% dplyr::summarise(drive_dist_diff  = mean(shot_dis..inch.))
    
    return( diff)
    
}





