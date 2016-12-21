#event mapping from various sources
#assume events.us is loaded

#get PGA source
pgaEvents <- read.table("./data/event_name_number.txt", sep=";", header=TRUE)

#extra  and rw column for some reason
pgaEvents <- pgaEvents[1:length(pgaEvents$year)-1,1:3]
pgaEvents$tourn_name <- as.character(pgaEvents$tourn_name)

#get edit distance for two strings, match tournament names based on that
getPGAInfoForEvent <- function(event){
  # given an event, search through PGA events for possible matches using levenshtein similarity
  # input: event
  # output: pga name and number of most likely matching
  
  #filter by year
  candidates <- pgaEvents[pgaEvents$year == event$year,]
  
  #calculate leven
  candidates.scores <-  unlist(lapply(candidates$tourn_name ,levenshteinSim, event$label))
  
  #threshold
  if(max(candidates.scores) > .8){
    candidates.winner <- candidates[which.max(candidates.scores),]
    return(candidates.winner[, c("tourn_number", "tourn_name")])
  }
  
  #no match, return NAs
  retList <- c(NA, NA)
  names(retList) <- c("tourn_number", "tourn_name")
  return(retList)
}


#do mappings and bind
events.usPGANames <- apply(events.us, 1, getPGAInfoForEvent)
events.us <- cbind(events.us, t(events.usPGANames))

