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
  
  #grab elevation from response json
  elevation <- elevation.json$results$elevation
  return(elevation)
}

getElevationForCourse <- function(course){
  # for a given course, check if it has proper attributes
  # if so, send lat&log through getElevation
  if(!is.na(course[["lat"]]) && !is.na(course[["lng"]])){
    return(getElevationAtPoint(course[["lat"]], course[["lng"]]))
  }
  return(NA)
}

elevations <- unlist(apply(events.us, 1,getElevationForCourse ))
events.us$elevations <- elevations