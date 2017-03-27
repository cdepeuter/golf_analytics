
#weather

# test getPGAEvents

test_that("All Events", expect_equal(nrow(getPGAEvents()) , 265))
test_that("2014-2015 Events", expect_equal(nrow(getPGAEvents(2014, 2015)), 75))
test_that("Tournament filter working", expect_equal(nrow(getPGAEvents(tournament = "BMW Championship")), 7))
test_that("Course filter working", expect_equal(nrow(getPGAEvents(course = "TPC River Highlands")), 7))
test_that("Combo params", expect_equal(nrow(getPGAEvents(2012, 2015, tournament = "John Deere Classic")), 4))


#test proviz file
test_that("Proviz exceptions", expect_equal(getProvizFileForCourse("Plantation Course at Kapalua"), "kapaluaplantation"))
test_that("Proviz regular", expect_equal(getProvizFileForCourse("Baltusrol GC"), "baltusrollower"))


# test elevation
test_that("elevations", expect_equal(getElevationAtPoints("40.744913874491,-73.452969145297|40.746631974663,-73.446830244683"), c(83.6151067201996, 144.5454170645142)))


#test coordinates / elevation
cc <- getCleanProvizForCourse("Congressional CC (Blue)")
test_that("congressional hole 10",  expect_equivalent(as.numeric(cc[10, c("hole_z", "hole_lat", "hole_lon")]), c(221.356001000061, 38.993882699388, -77.17630461763)))
test_that("congressional tee 5",  expect_equal(as.numeric(cc[5, c("tee_z", "tee_lat", "tee_lon")]), c(306.0845 , 39.000688500069, -77.183214818321), tolerance=.0005))
test_that("congressional mid 14",  expect_equivalent(as.numeric(cc[14, c("midpt1_z", "midpt1_lat", "midpt1_lon")]), c(234.728467748108, 38.990486099049, -77.173308517331)))

#torrey pines
tp <- getCleanProvizForCourse("Torrey Pines GC (South)")
test_that("torrey pines hole",  expect_equivalent(as.numeric(tp[1, c("hole_z", "hole_lat", "hole_lon")]), c(366.286517272644,32.902284290228 ,-117.24938372494)))
test_that("torrey pines tee",  expect_equal(as.numeric(tp[12, c("tee_z", "tee_lat", "tee_lon")]), c(372.177746685181, 32.890328689033, -117.24520322452)))
test_that("torrey pines min",  expect_equivalent(as.numeric(tp[9, c("midpt1_z", "midpt1_lat", "midpt1_lon")]), c(388.053420862732 , 32.897516089752, -117.24417632442)))


#assuming startup has been run

# weather function
safeway <- events[261,]

test_shot <-  safeway.shots[30195,]
safeway.weather <- getWeatherObsForTournament(safeway)
test_shot_weather <- shotWeatherSummary(test_shot[, "date_time"], safeway.weather)

test_that("1 hr before", expect_equal(test_shot_weather$rain_1_hrs_before, .0924))
test_that("2 hrs before", expect_equal(test_shot_weather$rain_2_hrs_before,  0.1024))
test_that("48 hrs before", expect_equal(test_shot_weather$rain_48_hrs_before,  0.4624))
test_that("mean wind", expect_equal(test_shot_weather$mean_wind_2hrs_before, 2.88333, tolerance = .00001))


id_to_name <- getPlayerIdMap(safeway.shots)
# shot angles

test_that("safeway first quad", expect_equal(getAimDegrees(safeway.shots[1,]), 15.65421,  tolerance = .00001))
test_that("safeway 4th quad", expect_equal(getAimDegrees(safeway.shots[6,]), 310.991,  tolerance = .00001))
test_that("safeway second quad", expect_equal(getAimDegrees(safeway.shots[16,]), 112.4111,  tolerance = .00001))
test_that("safeway third quad", expect_equal(getAimDegrees(safeway.shots[20,]), 197.8435,  tolerance = .00001))


# filtering shots
safeway.drives <- filterShots(safeway.shot_weather)
test_that("safeway mean drive dist", expect_equal(mean(safeway.drives$shot_dis..yards.), 275.5497, tolerance = .00001))


# player and hole groupings by round working

safeway.drives.by_player <- avgDistByRound(safeway.drives, "player")
safeway.drives.by_hole <- avgDistByRound(safeway.drives, "hole")

test_that("phil safeway, first day mean drive dist", expect_equal(mean(safeway.drives[safeway.drives$player == 1810 & safeway.drives$round == 1, "shot_dis..yards."]), 
                                                                  safeway.drives.by_player[safeway.drives.by_player$player == 1810, "r1_dist"]))

test_that("safeway hole 1 third day", expect_equal(mean(safeway.drives[safeway.drives$hole == 5 & safeway.drives$round == 3, "shot_dis..yards."]), safeway.drives.by_hole[safeway.drives.by_hole$hole == 5, "r3_dist"]))
