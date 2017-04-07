
#print("getting package from github")
#install_github("cdepeuter/golf_analytics")
#library(golfAnalysis)


#tourn_id <- as.integer(args[6])
#season <- as.integer(args[7])
#print(tourn_id)
#print(season)

events <- getPGAEvents()
events <- getLocationForEvents(events)


season <- 2016

# get relevant files
shotlink.directory <- "./data/shotlink"
allFiles <- list.files(shotlink.directory)
pattern <- paste0("^shot-ext-([a-z]*)-", season, "-(\\d+).txt$")
relevant.files <- allFiles[grepl(pattern, allFiles)]
print(relevant.files)

# get relevant events
relevant.events <- events[events$season == season, ]
#relevant.events <- events[238:260,]


print(length(relevant.events))
# make sure file num is same as events 
if(length(relevant.files) != length(relevant.events)){
    warning("files and events dont match")
}

shot_weathers_2016.2 <- by(relevant.events, 1:nrow(relevant.events), function(event){

    print(event)
    #print(paste(typeof(event[["local_tz"]]), typeof(event[["tourn"]]), typeof(event[["perm_tourn"]])))
    
    this_tourney.pattern <-  paste0("^shot-ext-([a-z]*)-", season, "-", event[["course"]], ".txt$")
    print(this_tourney.pattern)
    this_tourney.file <- allFiles[which(grepl(this_tourney.pattern, allFiles))]
    
    
    this_tourney.file_path <- paste0(shotlink.directory, "/" , this_tourney.file)
    print(paste("tournament file", this_tourney.file, this_tourney.file_path))
    
    if(!file.exists(this_tourney.file_path) | this_tourney.file_path == "./data/shotlink/"){
        print("NOO FILLEE")
    } else{
        # get this tournament shots
        print("getting shots")
        this_tourney.shots <- getShotlinkExtTable(this_tourney.file, event[["local_tz"]])
        
        # get this tournament weather
        this_tourney.weather <- getWeatherObsForTournament(event)
        
        print("matching shots and weather")
        # match shots and weather
        start.time  <- Sys.time()

        this_tourney.shot_weather <- matchWeatherToShots(this_tourney.shots, this_tourney.weather)
        end.time <- Sys.time()


        print(paste("time taken", as.character(end.time - start.time)))
        # get tournament summary
        
        # format for mark
        return(this_tourney.shot_weather)
    }
})


# make sure you're taking just the data frames
take_by <- lapply(shot_weathers_2016.2, typeof) == "list"

# bind together
all.shot.weather.2016.2 <- do.call("rbind", shot_weathers_2016.2[take_by])

