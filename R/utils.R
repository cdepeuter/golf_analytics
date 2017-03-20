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

getPlayerIdMap <- function(shots){
    # create a map from ID to player name
    player_names <- unique(paste(shots$player_first, shots$player_last))
    
    names(player_names) <- unique(shots$player)
    
    return(player_names)
}

getPlayerNameMap <- function(shots){
    player_ids <- unique(shots$player)
    
    names(player_ids) <- unique(paste(shots$player_first, shots$player_last))
    
    return(player_ids)
}
