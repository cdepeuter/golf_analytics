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
addDriveRegressionFeatures <- function(shots, use_just_long_holes){
    
    shots$drive_dist_diff <- driveDistDeviation(shots, use_just_long_holes)
    shots$net_wind <-  mapply(netWindVector, shots$wind_target_angle_diff, shots$last_wind_speed)
    shots$long_hole <- shots$dis_hole_start..yards. > long_hole_length
    
    #rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    
    return(shots)
}


netWindVector <- function(angle_diff, last_wind_speed){
    # 
    diff_from_0_360 <- 180 - abs(180 - angle_diff)
    cos_angle <- cos(diff_from_0_360 * pi / 180)
    
    return(last_wind_speed * cos_angle)
}

driveDistDeviation <- function(shots, use_just_long_holes){
    # gets average drive distance for each player, subtract
    # get the average for each player
    
    # only take shots no wind no rain
    #reg_shots <- shots[abs(shots$last_wind_speed) < 3 & shots$agg_48_hr_rain < .02 & shots$dis_hole_start..yards. > 450,]
    
    reg_shots <- shots[abs(shots$last_wind_speed) < 3 & shots$agg_48_hr_rain == 0,]
    
    if(use_just_long_holes){
        reg_shots <- shots[shots$dis_hole_start..yards. > long_hole_length,]
    }
    
    avg_by_player <- data.frame(reg_shots %>% group_by(player) %>% summarise(avg_dist = mean(shot_dis..yards.)))
    
    # make indexable by player
    rownames(avg_by_player) <- avg_by_player$player
    
    avg_dist_vector <- avg_by_player[as.character(shots$player), "avg_dist"]
    
    return(shots$shot_dis..yards. - avg_dist_vector)
}


rain_regression <- function(shots, isolate_interval){
    ## get regression coefficients for each rain stat, when all others are 0
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    all_rain_reg <- lapply(rain_cols, function(x, rain_c){
        # all other rains are 0
        #print(rain_c)
        shots <- shots[shots$long_hole,]
        
        
        if(isolate_interval){
            shots <- shots[shots[,"agg_48_hr_rain"] == shots[,x],]
        }
        
        lm_res <- lm(as.formula(paste("drive_dist_diff ~ ", x)), data = shots)
        #poly_res <- lm(as.formula(paste("drive_dist_diff ~ poly(", x, ", 2)")), data = isolated_shots)
        return(lm_res)
        #return(list(lm_res, poly_res))
    }, rain_cols)

    return(all_rain_reg)
}


# rain_lines <- function(shots){
#     ## get regression coefficients for each rain stat, when all others are 0
#     rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
#     
#     all_rain_reg <- lapply(rain_cols, function(x, rain_c){
#         # all other rains are 0
#         print(rain_c)
#         
#         isolated_shots <- shots[shots[,"agg_48_hr_rain"] == shots[,x],]
#         print(x)
#         print(dim(isolated_shots))
#         lm_res <- lm(as.formula(paste("drive_dist_diff ~ ", x)), data = isolated_shots)
#         #poly_res <- lm(as.formula(paste("drive_dist_diff ~ poly(", x, ", 2)")), data = isolated_shots)
#         return(lm_res)
#         #return(list(lm_res, poly_res))
#     }, rain_cols)
#     
#     return(all_rain_reg)
# }


add_adjusted_distance <- function(shots, wind_coeff, rain_coeffs){
    # add column which is drive distance accounting for wind and rain
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    to_adjust_wind <- shots$net_wind * wind_coeff
    to_adjust_rain <- as.matrix(shots[ ,rain_cols]) %*% matrix(rain_coeffs)
    
    shots$adjusted_dist <- shots$shot_dis..yards. - to_adjust_rain - to_adjust_wind
    
    return(shots)
}