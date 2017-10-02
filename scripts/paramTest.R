args <- commandArgs()
season <- as.integer(args[6])
course <- as.integer(args[7])
yearsBack <- as.integer(args[8])
weekRange <- as.integer(args[9])


#set defaults
if(is.na(weekRange)){
    weekRange <- 2
}
if(is.na(yearsBack)){
    yearsBack <- 5
}


print(paste("years", yearsBack, "   weeks", weekRange))

loc <- NA
date_ <- NA

print(paste("params", season, course))
if((season == 0) & (course == 0)){
    # use lat and lon
    loc <- as.character(args[10])
    date_ <- as.character(args[11])
    
    print(paste("getting historical weather for ", loc, date_))
    #historicalWeatherForEvent(NA, loc, date_, yearsBack, weekRange)
    
    
}else{
    print(paste("getting historical weather for season", season, "course", course))
   # historicalWeatherForEvent(thisEvent, yearsBack, weekRange)
    
    
    
}