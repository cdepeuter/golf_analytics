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
    
    reg_shots <- shots[ shots$agg_48_hr_rain == 0,]
    
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
        shots_ <- shots
        shots_ <- shots[shots_$long_hole,]
        
        
        if(isolate_interval){
            shots_ <- shots_[shots_[,"agg_48_hr_rain"] == shots_[,x],]
        }
        print(dim(shots_))
        
        lm_res <- lm(as.formula(paste("drive_dist_diff ~ ", x)), data = shots_)
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

rain_accumulation_time_distances <- function(shots, groupFactor = 2){

    rnded_1 <- data.frame(shots %>% group_by(round(rain_0_to_1_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=1, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_2 <- data.frame(shots %>% group_by(round(rain_1_to_2_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=2, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_4 <- data.frame(shots %>% group_by(round(rain_2_to_4_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=4, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_6 <- data.frame(shots %>% group_by(round(rain_4_to_6_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=6, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_8 <- data.frame(shots %>% group_by(round(rain_6_to_8_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=8, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    
    rnded_12 <- data.frame(shots %>% group_by(round(rain_8_to_12_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=12, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_18 <- data.frame(shots %>% group_by(round(rain_12_to_18_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=18, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_24 <- data.frame(shots %>% group_by(round(rain_18_to_24_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=24, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_36 <- data.frame(shots %>% group_by(round(rain_24_to_36_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=36, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    rnded_48 <- data.frame(shots %>% group_by(round(rain_36_to_48_hrs_before/groupFactor,digits=1)*groupFactor) %>% summarise(dist_diff = mean(drive_dist_diff, na.rm=TRUE), obs = n(), hrs=48, ster=sqrt(mean((drive_dist_diff - mean(drive_dist_diff, na.rm=TRUE))^2, na.rm = TRUE))))
    
    
    colnames(rnded_1) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_2) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_4) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_6) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_8) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_12) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_18) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_24) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_36) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    colnames(rnded_48) <- c("rain", "dist_diff", "obs", "hrs", "ster")
    
    
    rain_diff_by_hrs <- rbind(rnded_1, rnded_2, rnded_4, rnded_6, rnded_8, rnded_12, rnded_18, rnded_24, rnded_36, rnded_48)
    return(rain_diff_by_hrs)
}


add_adjusted_distance <- function(shots, wind_coeff, rain_coeffs){
    # add column which is drive distance accounting for wind and rain
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    to_adjust_wind <- shots$net_wind * wind_coeff
    to_adjust_rain <- as.vector(as.matrix(shots[ ,rain_cols]) %*% matrix(rain_coeffs))
    
    shots$adjusted_dist <- shots$shot_dis..yards. - to_adjust_rain - to_adjust_wind
    
    return(shots)
}