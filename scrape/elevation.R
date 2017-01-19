#https://maps.googleapis.com/maps/api/elevation/json?locations=39.7391536,-104.9847034&key=YOUR_API_KEY

# get elevation around each coordinate

# make sure each place has coordinates, check area around

# maybe somewhat supervised, draw lines for holes on google maps, 
# get coordinates in list, grab elevation for each coordinate

googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'


getElevationAtPoint <- function(lat, long){
  # given coordinates, get elevation for that point
  # input: coordinates
  # output: elevation in meters
  
  elevation.url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", lat, ",", long, "&key=", googleKey)
  
  #sometimes a space in elevations, remove from url
  elevation.url <- gsub(" ", "", elevation.url)
  
  print(paste("getting elevation at point", lat, long))
  #print(elevation.url)
  elevation.json <- elevation.url %>%
    GET() %>%
    content(as="text") %>%
    fromJSON()
  
  #grab elevation from response json, convert to feet
  elevation <- elevation.json$results$elevation
  return(elevation * 3.28084)
}

getElevationAtPoints <- function(pointString){
  # given string of points, give elevation at those points
  # input: string of lat,long separated by |
  # output: elevation list in meters
  
  elevation.url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", pointString, "&key=", googleKey)
  
  #sometimes a space in elevations, remove from url
  elevation.url <- gsub(" ", "", elevation.url)
  
  elevation.json <- elevation.url %>%
    GET() %>%
    content(as="text") %>%
    fromJSON()
  
  #grab elevation from response json, convert to feet
  elevation <- elevation.json$results$elevation
  return(elevation * 3.28084)
}

getElevationForCourse <- function(course){
  # for a given course, check if it has proper attributes
  # if so, send lat&log through getElevation
  if(!is.na(course[["lat"]]) && !is.na(course[["lng"]])){
    return(getElevationAtPoint(course[["lat"]], course[["lng"]]))
  }
  return(NA)
}

getGridAroundPoint <- function(lat, long, range = .030, steps = 10){
    # given a point, grab a grid of lat/longs around that point
    # input: lat/log of center point
    # output: matrix of elevation values around point
    scalee <- seq(from=range/-2, to=range/2, length.out = steps)
    
    latitudes <- lat + scalee
    longitudes <- long + scalee
    
    #get array in matrix
    pointArray <- matrix(unlist(lapply(latitudes, paste, longitudes, sep=",")), nrow=steps, ncol=steps) 
    return(pointArray)
}

gridElevationAroundPoint <- function(lat, long, range = .030, steps = 10){
  # given a point, grab a grid of elevations around that point
  # input: lat/log of center point
  # output: matrix of elevation values around point
  
  #get array in matrix
  pointArray <- getGridAroundPoint(lat, long, range, steps)
  pointStr <- paste(pointArray,  collapse="|")
  #print(pointArray)
  #print(pointStr)
  elevations <- getElevationAtPoints(pointStr)
  
  return(matrix(elevations, nrow=steps, ncol=steps))
}
