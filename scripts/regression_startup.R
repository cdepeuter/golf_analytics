long_hole_length <- 480
#all.shots2016 <- loadAndBindShotsForEvents(events[events$season == 2016,])
#all.w2016 <- read.table("data/shot-ext-weather-2016.txt", header = TRUE, sep=";")


#all.sw <- read.table("data/shots_complete-weather-ext-2016.txt", header=TRUE, sep=";")
#all.sw <- read.table("data/shots_complete-weather-ext-2015.txt", header=TRUE, sep=";")
all.sw <- read.table("data/shots_complete-weather-ext-2017.txt", header=TRUE, sep=";")

#just_weather <- read.table("data/shot-ext-weather-2015.txt", header=TRUE, sep=";")

all.drives <- filterShots(all.sw)

# all drives just long holes 
#all.drives <- all.drives[all.drives$dis_hole_start_yards > long_hole_length,]


all.drives <- addDriveRegressionFeatures(all.drives, TRUE)
all.drives <- all.drives[!is.na(all.drives$drive_dist_diff), ]


drive_dist_wind_fit <- lm(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0 & all.drives$long_hole,])
summary(drive_dist_wind_fit)

drive_dist_wind_velo_fit <- lm(drive_dist_diff ~ wind_vec, data = all.drives[all.drives$agg_48_hr_rain == 0 & all.drives$long_hole,])
summary(drive_dist_wind_velo_fit)


drive_dist_elevation_fit <- lm(drive_dist_diff ~ elevation_diff, data = all.drives[all.drives$long_hole,])
summary(drive_dist_elevation_fit)
#drive_dist_wind_fit <- lm.ridge(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0,])

# add adjusted wind
wind_coeff <- drive_dist_wind_fit$coefficients[["net_wind"]]
elevation_coeff <- drive_dist_elevation_fit$coefficients[["elevation_diff"]]


ggplot(all.drives[all.drives$agg_48_hr_rain == 0,], aes(x=net_wind, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm")



ggplot(all.drives, aes(x=elevation_diff, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm")


# rain regression at once
rain_dist_lm <- lm(drive_dist_diff ~ rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
                       rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
                       rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before, data = all.drives[abs(all.drives$net_wind) < 3 & all.drives$long_hole,])


# rain regression at once
rain_short_dist_lm <- lm(drive_dist_diff ~ rain_0_to_4_hrs_before + rain_4_to_12_hrs_before +rain_12_to_24_hrs_before +
                       rain_24_to_48_hrs_before, data = all.drives[abs(all.drives$net_wind) < 3 & all.drives$long_hole,])


# rain_dist_lm <- lm(drive_dist_diff ~ rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
#                        rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
#                        rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before, data = all.drives.long.holes[abs(all.drives.long.holes$net_wind) < 3,])

#all.drives.long.holes[all.drives.long.holes$agg_48_hr_rain == 0,]
summary(rain_dist_lm)

rain_dist_lm.coeffs <- rain_dist_lm$coefficients[2:length(rain_dist_lm$coefficients)]

# do regression for all rain values.  second param is to isolate only weather in that interval
all_rain_reg <- rain_regression(all.drives, isolate_interval =  TRUE)
plot_rain_regressions(all.drives, isolate_interval = TRUE)

#all_rain_reg <- rain_regression(all.drives.long.holes)
all_rain_reg.coeffs <- sapply(all_rain_reg, function(x){return(x[["coefficients"]][[2]])})
rain_cols <- colnames(all.drives)[grepl("rain_", colnames(all.drives))]
names(all_rain_reg.coeffs) <- rain_cols 

all.drives <- add_adjusted_distance(all.drives, wind_coeff, rain_dist_lm.coeffs, elevation_coeff)


# get specific players drives to run latent mixture model on
lovemark.drives <-  all.drives[all.drives$player == 29461,]
dj.drives <-  all.drives[all.drives$player == 30925,]
dufner.drives <-  all.drives[all.drives$player == 25686,]
kim.drives <-  all.drives[all.drives$player == 37455,]










