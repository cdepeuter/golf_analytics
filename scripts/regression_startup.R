long_hole_length <- 480
#all.shots2016 <- loadAndBindShotsForEvents(events[events$season == 2016,])
#all.w2016 <- read.table("shot-ext-weather-2016.txt", header = TRUE, sep=";")
all.sw2016 <- read.table("data/shots-weather-ext-2016", header=TRUE, sep=";")


lovemark.shots <- all.sw2016[all.sw2016$player == 29461,]
lovemark.drives <- filterShots(lovemark.shots)


dj.shots <- all.sw2016[all.sw2016$player == 30925,]
dj.drives <- filterShots(dj.shots)


dufner.shots <- all.sw2016[all.sw2016$player == 25686,]
dufner.drives <- filterShots(dufner.shots)


kim.shots <- all.sw2016[all.sw2016$player == 37455,]
kim.drives <- filterShots(kim.shots)


all.drives <- filterShots(all.sw2016)

# all drives just long holes 
#all.drives <- all.drives[all.drives$dis_hole_start..yards. > long_hole_length,]

all.drives$agg_48_hr_rain <- aggRain(all.drives)
all.drives <- addDriveRegressionFeatures(all.drives, TRUE)



drive_dist_wind_fit <- lm(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0,])

#drive_dist_wind_fit <- lm.ridge(drive_dist_diff ~ net_wind, data = all.drives[all.drives$agg_48_hr_rain == 0,])
summary(drive_dist_wind_fit)


# add adjusted wind
wind_incpt <- drive_dist_wind_fit$coefficients[["net_wind"]]



ggplot(all.drives[all.drives$agg_48_hr_rain == 0,], aes(x=net_wind, y=drive_dist_diff)) + geom_point() + geom_smooth(method="lm")



# rain regression at once
rain_dist_lm <- lm(drive_dist_diff ~ rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
                       rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
                       rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before, data = all.drives[abs(all.drives$net_wind) < 3,])
# rain_dist_lm <- lm(drive_dist_diff ~ rain_0_to_1_hrs_before + rain_1_to_2_hrs_before +rain_2_to_4_hrs_before +
#                        rain_4_to_6_hrs_before + rain_6_to_8_hrs_before + rain_8_to_12_hrs_before +
#                        rain_12_to_18_hrs_before + rain_18_to_24_hrs_before + rain_24_to_36_hrs_before + rain_36_to_48_hrs_before, data = all.drives.long.holes[abs(all.drives.long.holes$net_wind) < 3,])

#all.drives.long.holes[all.drives.long.holes$agg_48_hr_rain == 0,]
summary(rain_dist_lm)

rain_dist_lm.coeffs <- rain_dist_lm$coefficients[2:length(rain_dist_lm$coefficients)]

# do regression for all rain values.  second param is to isolate only weather in that interval
all_rain_reg <- rain_regression(all.drives, FALSE)
plot_rain_regressions(all.drives, FALSE)

#all_rain_reg <- rain_regression(all.drives.long.holes)
all_rain_reg.coeffs <- sapply(all_rain_reg, function(x){return(x[["coefficients"]][[2]])})
rain_cols <- colnames(all.drives)[grepl("rain_", colnames(all.drives))]
names(all_rain_reg.coeffs) <- rain_cols 

all.drives <- add_adjusted_distance(all.drives, wind_incpt, rain_dist_lm.coeffs)

## plots

ggplot(all.drives[abs(all.drives$net_wind) < 3 & all.drives$rain_0_to_1_hrs_before != 0,], aes(x=rain_0_to_1_hrs_before, y=drive_dist_diff)) + geom_point()  +
 stat_smooth(method="lm", formula = y ~ x) + stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE, position=position_jitter(width=.5))


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_1_to_2_hrs_before, y=drive_dist_diff)) + geom_point() +
    stat_smooth(method="lm", formula = y ~ x) + stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_2_to_4_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) +
  stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_4_to_6_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) +
+  stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_6_to_8_hrs_before, y=drive_dist_diff)) + geom_point() +
 stat_smooth(method="lm", formula = y ~ x)  + stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_8_to_12_hrs_before, y=drive_dist_diff)) + geom_point() + 
  stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_12_to_18_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) +
 + stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_18_to_24_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) 
+ stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)

ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_24_to_36_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) + 
    stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)


ggplot(all.drives[abs(all.drives$net_wind) < 3,], aes(x=rain_36_to_48_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) +
    stat_poly_eq(formula=y~x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE)



# plots, rain != 0
ggplot(all.drives[abs(all.drives$net_wind) < 3 & all.drives$rain_0_to_1_hrs_before != 0,], aes(x=rain_0_to_1_hrs_before, y=drive_dist_diff)) + geom_point() + stat_smooth(method="lm", formula = y ~ x) + stat_poly_eq(formula=y~x, parse=TRUE)

# plots for just long holes


# no rain shots
