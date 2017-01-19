
pin1 <- courseData[1,]
    
pts <- getGridAroundPoint(as.integer(pin1$lat), as.integer(pin1$lon[1]))
lats <- unique(as.double(unlist(lapply(as.list(pts), function(x){return(strsplit(x, ",")[[1]][1])}))))
lns <- unique(as.double(unlist(lapply(as.list(pts), function(x){return(strsplit(x, ",")[[1]][2])}))))


z <- gridElevationAroundPoint(as.integer(pin1$lat), as.integer(pin1$lon[1]))

debug.print(paste("lengths", length(lats), length(lns), length(z)))

persp(lats, lns, z,
      xlab = "Latitude", ylab = "Longitude",
      main = "Surface elevation data"
)

