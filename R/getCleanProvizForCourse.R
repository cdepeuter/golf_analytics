#' Get Proviz in the cleaned format 
#'
#' This function returns a data frame of events for the pga tour between specific years, or at specific courses/tournaments
#' @param event event returned
#' @keywords pga golf
#' @return Data Frame of events for the given year range.
#' @export
#' @examples getEventsForYears(2010, 2015)
#' 
#' 



getCleanProvizForCourse <- function(courseName){
    course <- paste0(getProvizFileForCourse(courseName), ".php")
    
    course.url <- paste0("http://www.provisualizer.com/courses/", course)
    debug.print(course.url)
    course.html <- getUrlResponse(course.url)
    course.id <- getIDFromResp(course.html)
    if(! is.na(course.id)){
        course.3dUrl <- getCourse3dUrl(paste0("https://www.provisualizer.com/3dlink.php?id=", course.id))
        debug.print(course.3dUrl)
        course.points <- getHolePointsForCourse(course.3dUrl)
        course.points$link <- course.url
        course.points$course <- courseName
        
        #re-arrange cols
        frontNames <- c("course", "link")
        course.points <- course.points[, c(frontNames, colnames(holeData)[!colnames(holeData) %in% frontNames])]
        return(course.points)
    } else{
        debug.print(paste("na course id, skipping", courseName))
    }
    
    
}

