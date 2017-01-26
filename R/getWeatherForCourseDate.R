#' On a given day/course get the weather observations from a local file
#'
#' This function loads the weather json from a local directory and writes summary information for that day
#' @param string input name of course
#' @param string date of obs
#' @return summary info for event
#' @export
#' @examples
#'getWeatherForCourseDate("TPC Scottsdale")



getWeatherForCourseDate <- function(course, dateStr){
    # get weather info json response for event at given address on date
    # input: course info with city, state, zip(maybe)
    # output json response from weather underground api
    
    # DONT run this function in vectorized format on an array
    # weatherUnderground maxes API calls at 10 per minute
    
    #get weather and addr for filename
    wugDateStr <- dateStr
    
    addr <- paste(course[["lat"]], course[["lng"]], sep=",") 
    
    
    filename <- paste0("./data/weather/",addr, "-", wugDateStr, ".json")
    filename <- gsub(" ", "", filename)
 
    
    if(!file.exists(filename)){
        stop("NO LOCAL FILE")
    }
    
    
    weatherContent <- read_file(filename)
    weatherJSON <- jsonlite::fromJSON(weatherContent)
    observations <- weatherJSON$history$observations
    
    # TODO CHECK TIMEZONE DATA
    # maybe use UTC if this is an issue
    if(weatherJSON$history$date$tzname !="America/New_York"){
        print("TIMEZONE NOT STANDARD");
    }
    
    
    #remove nested dataframe and add info in separate columns
    dateData <- observations$date
    hr <- observations$date$hour
    min  <- observations$date$min
    time <- as.integer(paste(hr, min, sep=""))
    
    
    year <- observations$date$year
    month <- observations$date$mon
    day <- observations$date$mday
    
    date <- paste(year, month, day, sep="")
    
    observations$date <- date
    observations$time <- time
    
    #drop utc nested dataframe, other column which is probably meter name, 
    observations <- observations[, !(colnames(observations) %in% c("utcdate" ))]
    
    return(observations)
}
