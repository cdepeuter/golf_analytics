# par 5s and long par 4s
# - fairway or first cut
# - player I average distance on these shots
# - distance deviation from player average
# - dist ~ avg + wind(speed, direction) + rain
# 
# plots: no wind, 0 rain,
# no wind + rain
# 0 rain, 0 wind,
# 0 rain, + wind
# 
# for wind + rain
# 
# cos(theta)* windSpeed = relative wind speed

# 
addDriveRegressionFeatures <- function(shots){
    
    shots$drive_dist_diff <- driveDistDeviation(shots)
    shots$net_wind <-  mapply(netWindVector, shots$wind_target_angle_diff, shots$last_wind_speed)
    
    return(shots)
}


netWindVector <- function(angle_diff, last_wind_speed){
    # 
    diff_from_0_360 <- 180 - abs(180 - angle_diff)
    cos_angle <- cos(diff_from_0_360 * pi / 180)
    
    return(last_wind_speed * cos_angle)
}

driveDistDeviation <- function(shots){
    # gets average drive distance for each player, subtract
    # get the average for each player
    
    # only take shots no wind no rain
    reg_shots <- shots[abs(shots$last_wind_speed) < 3 & shots$agg_48_hr_rain < .02,]
    
    avg_by_player <- data.frame(reg_shots %>% group_by(player) %>% summarise(avg_dist = mean(shot_dis..yards.)))
    
    # make indexable by player
    rownames(avg_by_player) <- avg_by_player$player
    
    avg_dist_vector <- avg_by_player[as.character(shots$player), "avg_dist"]
    
    return(shots$shot_dis..yards. - avg_dist_vector)
}


rain_regression <- function(shots){
    ## get regression coefficients for each rain stat, when all others are 0
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    all_rain_reg <- lapply(rain_cols, function(x){
        # all other rains are 0
        isolated_shots <- shots[shots[,rain_cols[!grepl(x, rain_cols)]] == 0,]
        print(x)
        print(dim(isolated_shots))
        lm_res <- lm(as.formula(paste("drive_dist_diff ~ ", x)), data = isolated_shots)
        return(lm_res)
    })

    return(all_rain_reg)
}

add_adjusted_distance <- function(shots, wind_coeff, rain_coeffs){
    # add column which is drive distance accounting for wind and rain
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    to_adjust_wind <- shots$net_wind * wind_coeff
    to_adjust_rain <- as.matrix(shots[ ,rain_cols]) %*% matrix(rain_coeffs)
    
    shots$adjusted_dist <- shots$shot_dis..yards. - to_adjust_rain - to_adjust_wind
    
    return(shots)
}