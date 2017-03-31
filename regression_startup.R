
all.shots2016 <- loadAndBindShotsForEvents(events[events$season == 2016,])
all.w2016 <- read.table("shot-ext-weather-2016.txt", header = TRUE, sep=";")
all.sw2016 <- cbind(all.shots2016, all.w2016[,!colnames(all.w2016) %in% colnames(all.shots2016)])


lovemark.shots <- all.sw2016[all.sw2016$player == 29461,]
lovemark.drives <- filterShots(lovemark.shots)


dj.shots <- all.sw2016[all.sw2016$player == 30925,]
dj.drives <- filterShots(dj.shots)


dufner.shots <- all.sw2016[all.sw2016$player == 25686,]
dufner.drives <- filterShots(dufner.shots)


kim.shots <- all.sw2016[all.sw2016$player == 37455,]
kim.drives <- filterShots(kim.shots)


all.drives <- filterShots(all.sw2016)

all.drives$agg_48_hr_rain <- aggRain(all.drives)
all.drives <- addDriveRegressionFeatures(all.drives)



drive_dist_wind_fit <- lm(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0,])
#drive_dist_wind_fit <- lm.ridge(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0,])
summary(drive_dist_wind_fit)


# add adjusted wind
wind_incpt <- drive_dist_wind_fit$coefficients[["net_wind"]]




ggplot(all.drives[all.drives$rain_0_to_1_hrs_before == 0 & all.drives$rain_1_to_2_hrs_before ==0 & all.drives$rain_2_to_4_hrs_before == 0,], aes(x=net_wind, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm")



# rain regression at once
rain_dist_lm <- lm(drive_dist_diff ~ rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
                       rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
                       rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before, data = all.drives[abs(all.drives$net_wind) < 3,])
summary(rain_dist_lm)

rain_dist_lm.coeffs <- rain_dist_lm$coefficients[2:length(rain_dist_lm$coefficients)]

# do regression for all rain values individually
all_rain_reg <- rain_regression(all.drives)
all_rain_reg.coeffs <- sapply(all_rain_reg, function(x){return(x[["coefficients"]][[2]])})
rain_cols <- colnames(all.drives)[grepl("rain_", colnames(all.drives))]
names(all_rain_reg.coeffs) <- rain_cols 

all.drives <- add_adjusted_distance(all.drives, wind_incpt, rain_dist_lm.coeffs)

## plots


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_0_to_1_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_1_to_2_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_2_to_4_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_4_to_6_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_6_to_8_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_8_to_12_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_12_to_18_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_18_to_24_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_24_to_36_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 
ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_36_to_48_hrs_before, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm") 

