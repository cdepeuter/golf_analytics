#tests
#courses to test on
pga <- getEventByEventName(events.us, "PGA Championship")
murfield <- getEventByCourseName(events.us, "Muirfield Village GC")
masters <- getEventByEventName(events.us, "Masters Tournament")
summerlin <- getEventByCourseName(events.us, "TPC Summerlin")
barclays <- getEventByEventName(events.us, "The Barclays")



#latitude/longitude
pga.coordinates <- c(40.70502,-74.32799)
names(pga.coordinates) <- c("lat", "lng")
test_that("PGA Champ coordinates", expect_equal(getLatLongByPlaceName(pga), pga.coordinates, tolerance=1e-5))

murfield.coordinates <- c(40.1404806,-83.140923)
names(murfield.coordinates) <- c("lat", "lng")
test_that("Murfield Champ coordinates", expect_equal(getLatLongByPlaceName(murfield), murfield.coordinates, tolerance=1e-5))

masters.coordinates <- c(33.5021365, -82.0226276)
names(masters.coordinates) <- c("lat", "lng")
test_that("Masters coordinates", expect_equal(getLatLongByPlaceName(masters), masters.coordinates, tolerance=1e-5))

summerlin.coordinates <- c(36.1884284, -115.2982117)
names(summerlin.coordinates) <- c("lat", "lng")
test_that("Summerlin coordinates", expect_equal(getLatLongByPlaceName(summerlin), summerlin.coordinates, tolerance=1e-5))

barclays.coordinates <- c(40.7451112, -73.4613274)
names(barclays.coordinates) <- c("lat", "lng")
test_that("Barclays coordinates", expect_equal(getLatLongByPlaceName(barclays), barclays.coordinates, tolerance=1e-5))


#elevation
pga.elevation <- 165.6385
test_that("PGA Elevation", expect_equal(getElevationForCourse(pga), pga.elevation, tolerance=1e-5))

murfield.elevation <- 932.169566109619
test_that("PGA Champ Elevation", expect_equal(getElevationForCourse(murfield), murfield.elevation, tolerance=1e-5))

masters.elevation <- 277.426
test_that("Masters Elevation", expect_equal(getElevationForCourse(masters), masters.elevation, tolerance=1e-5))

summerlin.elevation <- 2715.97102488525
test_that("Summerlin Elevation", expect_equal(getElevationForCourse(summerlin), summerlin.elevation, tolerance=1e-5))

barclays.elevation <- 116.4917
test_that("Barclays Elevation", expect_equal(getElevationForCourse(barclays), barclays.elevation, tolerance=1e-5))



