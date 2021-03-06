safeway.weathers <- do.call("rbind.data.frame", apply(safeway.shots, 1, shotWeatherSummary, safeway.weather))
safeway.shot_weather <- cbind.data.frame(safeway.shots, safeway.weathers)

deg_wind_diff <- safeway.shot_weather$shot_degrees - safeway.shot_weather$last_wind_dir_degrees
deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
safeway.shot_weather$wind_shot_diff <- round(deg_wind_diff, digits=-1)


safeway.drives <- safeway.shot_weather[safeway.shot_weather$shot_num == 1 & safeway.shot_weather$par > 3 & safeway.shot_weather$loc_end == 4 &safeway.shot_weather$last_wind_speed > 2,]
safeway.drives_v_wind <- safeway.drives %>% group_by(wind_shot_diff) %>% summarise(drive_dist = mean(shot_dis_yards))

ggplot(safeway.drives_v_wind, aes(x=wind_shot_diff)) + geom_line(aes(y=drive_dist))


# do for three tournaments


three_toruney_shots <- rbind(safeway.shots, shriners.shots, sanderson.shots)
three_tourney_weather <- rbind(safeway.weather, shriners.weather, sanderson.weather)

three_tourney_weathers <- do.call("rbind.data.frame", apply(three_toruney_shots, 1, shotWeatherSummary, three_tourney_weather))

three_tourney_shot_weather <- cbind(three_toruney_shots, three_tourney_weathers)
deg_wind_diff <- three_tourney_shot_weather$shot_degrees - three_tourney_shot_weather$last_wind_dir_degrees
deg_wind_diff[deg_wind_diff < 0] <- deg_wind_diff[deg_wind_diff < 0] + 360
three_tourney_shot_weather$wind_shot_diff <- round(deg_wind_diff, digits=-1)

three_tourney_drives <- three_tourney_shot_weather[three_tourney_shot_weather$shot_num == 1 & three_tourney_shot_weather$par > 3 & three_tourney_shot_weather$loc_end == 4 & three_tourney_shot_weather$last_wind_speed > 6,]

three_tourney_drives_v_wind <- three_tourney_drives %>% group_by(wind_shot_diff) %>% summarise(drive_dist = mean(shot_dis_yards))
ggplot(three_tourney_drives_v_wind, aes(x=wind_shot_diff)) + geom_line(aes(y=drive_dist))

