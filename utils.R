getEventByEventName <- function(events, eventName){
    return(events[which(events$label == eventName), ])
}

getEventByCourseName <- function(events, courseName){
    return(events[which(events$courseName == courseName), ])
}