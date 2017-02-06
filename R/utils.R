getEventByEventName <- function(events, eventName){
    return(events[which(events$label == eventName), ])
}

getEventByCourseName <- function(events, courseName){
    return(events[which(events$courseName == courseName), ])
}

getShotId <- function(shot){
    #get unique identifier for shots
    sid <- paste(shot[["Player.."]],shot[["Tourn.."]], shot[["Course.."]], shot[["Round"]], shot[["Hole"]], shot[["Shot"]], sep="-")
    return(gsub(" ", "", sid))
}


