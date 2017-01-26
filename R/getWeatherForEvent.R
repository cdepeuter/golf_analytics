#' Get a summary of weather for an event
#'
#' This function finds summary weather data for an individual golf event
#' @param event from getPGAEvents() function
#' @return Dataframe of summary information from event
#' @export
#' @examples
#' getWeatherForTournament()


getWeatherForTournament <- function(event){

    
    # get weather for 3 days before tournament
    dates <- seq.Date(as.Date(course[["start"]])-3, as.Date(event[["end"]]), by="day")
    
    ret <- list()
    ret[1] <- course[["course.1"]]
    ret[2] <- course[["tourn"]]
    
    #getWeatherObservationsForCourseDate
    nDates <- length(dates)
    
    #hope 6 is the max number of days in tourney
    for(i in 1:9){
        if(i <= length(dates)){
            date <- dates[i]
            
            
            #file should save, thats what we really want
            #but from this response check out airport code
            wObs <- getWeatherObservationsForCourseDate(course, date)
            nObs <- dim(wObs)[1]
            
            ret[i+2] <- nObs
            
            #TODO 5 days uh oh
        }else{
            ret[i+2] <- NA
        }
    }
    
    #keep track of metar/airport code
    air <- substring(strsplit(wObs$metar[1], split = " ")[[1]][[2]], 2)
    debug.print(paste("airport code", air)) 
    
    ret[12] <- air
    
    #getAirport location
    airLoc <- getAirportLocation(air)
    
    #swap lat/long
    airLoc <- as.vector(c(airLoc[2], airLoc[1]))
    courseLoc <- as.vector(as.double(c(course[["lng"]], course[["lat"]])))
    
    #get Distance convert to miles
    debug.print(courseLoc)
    debug.print(airLoc)
    if(is.null(courseLoc) | is.null(airLoc)){
        dist <- NA
    }else{
        dist <- distVincentySphere(airLoc, courseLoc) * 0.000621371 
    }
    
    ret[13] <- dist
    
    debug.print(paste("Distance between course and measurements", dist))
    
    return(ret)
}