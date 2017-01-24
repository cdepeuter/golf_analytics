debug.debug=TRUE
debug.print <- function(x){if(debug.debug){print(x)}}

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

getUrlResponse <- function(url){
    # input : url
    # output html repsponse for 
    html <- url %>%
        GET() %>%
        content(as="text")
    return(html)
}

getProvizForCourse <- function(course){
    candidates <- list.files("./data/proviz/")
    candidates.scores <-  unlist(lapply(candidates ,function(x, course){
            x <- gsub("proviz-", "", x)
            c <- gsub(" ", "", course)
            c <- gsub("[(]|[)]", "", course)

            return(levenshteinSim(x, course))
        
        }, course))
    
    return(c( candidates[which.max(candidates.scores)], max(candidates.scores)))
}