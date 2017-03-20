
season <- 2017

# get relevant files
shotlink.directory <- "./data/shotlink"
allFiles <- list.files(shotlink.directory)
pattern <- paste0("^shot-ext-([a-z]*)-", season, ".txt$")
relevant.files <- grepl(pattern, allFiles)

# get relevant events
relevant.events <- events[events$season == season, ]


# make sure file num is same as events 
if(length(relevant.files) != length(relevant.events)){
    warning("files and events dont match")
}

shot_weathers <- by(relevant.events, 1:nrow(relevant.events), function(x){

    print(x)
    print(paste(typeof(x[["local_tz"]]), typeof(x[["tourn"]]), typeof(x[["perm_tourn"]])))
    
    
    # get this tournament shots
    
    # get this tournament weather
    this_tourney_weather <- getWeatherObsForTournament(x)
    
    
    # match shots and weather
    
    # format for mark
    return("hi")
})