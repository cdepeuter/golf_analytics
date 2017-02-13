#' Get coordinates for pga course
#'
#' This function finds coordinates for a course name
#' @param string course, name of course
#' @return vector of c(lat, long)
#' @export
#' @examples
#' getLocationForPGACourse("Baltusrol GC")


getLocationForPGACourse <- function(course){
    
    # check lookup first
    lookup <- read.table("./data/pga-hole-coords.csv", header = TRUE, stringsAsFactors = FALSE, sep=",")
    if(length(which(lookup$course == course)) > 0 ){
        return(lookup[which(lookup$course== course)[1],4:5])
    }
    
    #else get through google maps
    
    #google places api has a much smaller limit, can only do 100 of these requests a day 
    maps.place <- gsub(" ", "+",  course)
    maps.url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",maps.place, "&key=AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg")
    
    maps.json <- jsonlite::fromJSON(getUrlResponse(maps.url))
    
    if(length(maps.json$results) == 0){
        debug.print(paste("No location for place", course))
        maps.latlong <- c(NA, NA)
    }else{
        maps.latlong <- maps.json$results$geometry$location[1,]
    }
    
    return(unlist(maps.latlong))
}