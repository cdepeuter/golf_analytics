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


collapseRainCols <- function(shots){
    old_rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    shots$rain_0_to_4_hrs_before <- shots$rain_0_to_1_hrs_before + shots$rain_1_to_2_hrs_before + shots$rain_2_to_4_hrs_before
    shots$rain_4_to_12_hrs_before <- shots$rain_4_to_6_hrs_before + shots$rain_6_to_8_hrs_before + shots$rain_8_to_12_hrs_before
    shots$rain_12_to_24_hrs_before <- shots$rain_12_to_18_hrs_before + shots$rain_18_to_24_hrs_before 
    shots$rain_24_to_48_hrs_before <- shots$rain_24_to_36_hrs_before + shots$rain_36_to_48_hrs_before 
    
    shots <- shots[,!colnames(shots) %in% old_rain_cols]
    return(shots)
}


addDriveRegressionFeatures <- function(shots, use_just_long_holes){
    
    shots$drive_dist_diff <- driveDistDeviation(shots, use_just_long_holes)
    shots$net_wind <-  mapply(netWindVector, shots$wind_target_angle_diff, shots$last_wind_speed)
    #shots$net_wind <-  mapply(netWindVector, shots$wind_target_angle_diff, shots$mean_wind_2hrs_before)
    
    shots$long_hole <- shots$dis_hole_start_yards > long_hole_length
    shots$elevation_diff <- shots$start_z_yards - shots$end_z_yards
    #rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    
    return(shots)
}


netWindVector <- function(angle_diff, last_wind_speed){

    diff_from_0_360 <- 180 - abs(180 - angle_diff)
    cos_angle <- cos(diff_from_0_360 * pi / 180)
    
    return(last_wind_speed * cos_angle)
}

driveDistDeviation <- function(shots, use_just_long_holes){
    # gets average drive distance for each player, subtract
    # get the average for each player
    
    # only take shots no wind no rain
    #reg_shots <- shots[abs(shots$last_wind_speed) < 3 & shots$agg_48_hr_rain < .02 & shots$dis_hole_start_yards > 450,]
    
    reg_shots <- shots[ shots$agg_48_hr_rain == 0,]
    
    if(use_just_long_holes){
        reg_shots <- shots[shots$dis_hole_start_yards > long_hole_length,]
    }
    
    avg_by_player <- data.frame(reg_shots %>% group_by(player) %>% summarise(avg_dist = mean(shot_dis_yards)))
    
    # make indexable by player
    rownames(avg_by_player) <- avg_by_player$player
    
    avg_dist_vector <- avg_by_player[as.character(shots$player), "avg_dist"]
    print(length(avg_dist_vector))
    return(shots$shot_dis_yards - avg_dist_vector)
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


add_adjusted_distance <- function(shots, wind_coeff, rain_coeffs, elevation_coeff){
    # add column which is drive distance accounting for wind and rain
    rain_cols <- colnames(shots)[grepl("rain_", colnames(shots))]
    
    to_adjust_wind <- shots$net_wind * wind_coeff
    to_adjust_rain <- as.vector(as.matrix(shots[ ,rain_cols]) %*% matrix(rain_coeffs))
    to_adjust_elevation <- shots$elevation_diff * elevation_coeff
    
    shots$adjusted_dist <- shots$shot_dis_yards - to_adjust_rain - to_adjust_wind - to_adjust_elevation
    
    return(shots)
}


#' Generate yearly summary report of statistics for holes, hope to identify interesting ones for analysis
#'
#' This function calculates and saves info for a tournament
#' @param shot_weather dataframe of shots and weather
#' @return saved info
#' @export
#' @import flexmix
#' @import ggpmisc
#' @import ggplot2
#' @examples
#' yearlyReport(sw)


yearlyReport <- function(shot_weather){
    long_hole_length <<- 480
    just.drives <- filterShots(shot_weather)
    
    # all drives just long holes 
    #all.drives <- all.drives[all.drives$dis_hole_start_yards > long_hole_length,]
    
    
    just.drives <- addDriveRegressionFeatures(just.drives, TRUE)
    just.drives <- just.drives[!is.na(just.drives$drive_dist_diff), ]
    
    just.drives <- collapseRainCols(just.drives)
    classes <-  flexmix(drive_dist_diff ~ net_wind + elevation_diff + rain_0_to_4_hrs_before + rain_4_to_12_hrs_before +rain_12_to_24_hrs_before +
                            rain_24_to_48_hrs_before ,  just.drives, k=2)
    
    
    flex_results <- clusters(classes)
    just.drives$club_class <- flex_results

    club_choice_by_course_hole <- just.drives %>% group_by( course, hole) %>% summarise(estimated_pct_long_club = mean(club_class == 2), 
                                                                                       obs = n(), 
                                                                                       elevation_diff = mean(elevation_diff),
                                                                                       season = season[1],
                                                                                       rnd_0_mean_dist_long_club = mean(shot_dis_yards[club_class == 2]),
                                                                                       rnd_0_std_dist_long_club = sqrt(var(shot_dis_yards[club_class == 2])),
                                                                                       rnd_0_mean_dist_short_club = mean(shot_dis_yards[club_class != 2]),
                                                                                       rnd_0_std_dist_short_club = sqrt(var(shot_dis_yards[club_class != 2])),
                                                                                       rnd_1_mean_dist_long_club = mean(shot_dis_yards[(club_class == 2) & (round == 1)]),
                                                                                       rnd_1_std_dist_long_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 1)])),
                                                                                       rnd_1_mean_dist_short_club = mean(shot_dis_yards[(club_class == 2) & (round == 1)]),
                                                                                       rnd_1_std_dist_short_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 1)])),
                                                                                       rnd_2_mean_dist_long_club = mean(shot_dis_yards[(club_class == 2) & (round == 2)]),
                                                                                       rnd_2_std_dist_long_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 2)])),
                                                                                       rnd_2_mean_dist_short_club = mean(shot_dis_yards[(club_class == 2) & (round == 2)]),
                                                                                       rnd_2_std_dist_short_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 2)])),
                                                                                       rnd_3_mean_dist_long_club = mean(shot_dis_yards[(club_class == 2) & (round == 3)]),
                                                                                       rnd_3_std_dist_long_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 3)])),
                                                                                       rnd_3_mean_dist_short_club = mean(shot_dis_yards[(club_class == 2) & (round == 3)]),
                                                                                       rnd_3_std_dist_short_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 3)])),
                                                                                       rnd_4_mean_dist_long_club = mean(shot_dis_yards[(club_class == 2) & (round == 4)]),
                                                                                       rnd_4_std_dist_long_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 4)])),
                                                                                       rnd_4_mean_dist_short_club = mean(shot_dis_yards[(club_class == 2) & (round == 4)]),
                                                                                       rnd_4_std_dist_short_club = sqrt(var(shot_dis_yards[(club_class == 2) & (round == 4)])),
                                                                                       rnd_1_avg_dist = mean(shot_dis_yards[round == 1]),
                                                                                       rnd_2_avg_dist = mean(shot_dis_yards[round == 2]),
                                                                                       rnd_3_avg_dist = mean(shot_dis_yards[round == 3]),
                                                                                       rnd_4_avg_dist = mean(shot_dis_yards[round == 4]),
                                                                                       rnd_1_rain = mean(rain_0_to_4_hrs_before[round == 1] + rain_4_to_12_hrs_before[round == 1] + rain_12_to_24_hrs_before[round == 1]),
                                                                                       rnd_2_rain = mean(rain_0_to_4_hrs_before[round == 2] + rain_4_to_12_hrs_before[round == 2] + rain_12_to_24_hrs_before[round == 2]),
                                                                                       rnd_3_rain = mean(rain_0_to_4_hrs_before[round == 3] + rain_4_to_12_hrs_before[round == 3] + rain_12_to_24_hrs_before[round == 3]),
                                                                                       rnd_4_rain = mean(rain_0_to_4_hrs_before[round == 4] + rain_4_to_12_hrs_before[round == 4] + rain_12_to_24_hrs_before[round == 4]),
                                                                                       rnd_0_avg_wind_speed = mean(last_wind_speed, na.rm=TRUE),
                                                                                       rnd_1_avg_wind_speed = mean(last_wind_speed[round == 1], na.rm=TRUE),
                                                                                       rnd_2_avg_wind_speed = mean(last_wind_speed[round == 2], na.rm=TRUE),
                                                                                       rnd_3_avg_wind_speed = mean(last_wind_speed[round == 3], na.rm=TRUE),
                                                                                       rnd_4_avg_wind_speed = mean(last_wind_speed[round == 4], na.rm=TRUE),
                                                                                       rnd_0_max_wind_speed = max(last_wind_speed, na.rm=TRUE),
                                                                                       rnd_1_max_wind_speed = max(last_wind_speed[round == 1], na.rm=TRUE),
                                                                                       rnd_2_max_wind_speed = max(last_wind_speed[round == 2], na.rm=TRUE),
                                                                                       rnd_3_max_wind_speed = max(last_wind_speed[round == 3], na.rm=TRUE),
                                                                                       rnd_4_max_wind_speed = max(last_wind_speed[round == 4], na.rm=TRUE),
                                                                                       rnd_0_wind_dir_mean = yamartino(last_wind_dir_degrees),
                                                                                       rnd_0_wind_dir_std = yamartino_std(last_wind_dir_degrees),
                                                                                       rnd_1_wind_dir_mean = yamartino(last_wind_dir_degrees[round == 1]),
                                                                                       rnd_1_wind_dir_std = yamartino_std(last_wind_dir_degrees[round == 1]),
                                                                                       rnd_2_wind_dir_mean = yamartino(last_wind_dir_degrees[round == 2]),
                                                                                       rnd_2_wind_dir_std = yamartino_std(last_wind_dir_degrees[round == 2]),
                                                                                       rnd_3_wind_dir_mean = yamartino(last_wind_dir_degrees[round == 3]),
                                                                                       rnd_3_wind_dir_std = yamartino_std(last_wind_dir_degrees[round == 3]),
                                                                                       rnd_4_wind_dir_mean = yamartino(last_wind_dir_degrees[round == 4]),
                                                                                       rnd_4_wind_dir_std = yamartino_std(last_wind_dir_degrees[round == 4]),
                                                                                       rnd_1_wind_v_shots_mean = yamartino(wind_target_angle_diff[round == 1]),
                                                                                       rnd_1_wind_v_shots_std = yamartino_std(wind_target_angle_diff[round == 1]),
                                                                                       rnd_2_wind_v_shots_mean = yamartino(wind_target_angle_diff[round == 2]),
                                                                                       rnd_2_wind_v_shots_std = yamartino_std(wind_target_angle_diff[round == 2]),
                                                                                       rnd_3_wind_v_shots_mean = yamartino(wind_target_angle_diff[round == 3]),
                                                                                       rnd_3_wind_v_shots_std = yamartino_std(wind_target_angle_diff[round == 3]),
                                                                                       rnd_4_wind_v_shots_mean = yamartino(wind_target_angle_diff[round == 4]),
                                                                                       rnd_4_wind_v_shots_std = yamartino_std(wind_target_angle_diff[round == 4])
                                                                                      
                                                                                       
    )
    
    # fill na and write
    club_choice_by_course_hole[is.na(club_choice_by_course_hole)] <- -9999
    write.table(club_choice_by_course_hole, paste0("./data/", just.drives$season[1], "weather_regression_summary.txt"),  sep = ";", row.names = FALSE)
}


