
#print("getting package from github")
#install_github("cdepeuter/golf_analytics")
#library(golfAnalysis)


#tourn_id <- as.integer(args[6])
#season <- as.integer(args[7])
#print(tourn_id)
#print(season)

events <- getPGAEvents()
events <- getLocationForEvents(events)


season <- 2017

# do we already have a fule for these events?
weather_shots_file_name <- paste0("./data/shots_complete-weather-ext-", season, ".txt")
weather_file_name <- paste0("./data/shot-ext-weather-", season, ".txt")
events_we_have <- c()

if(file.exists(weather_file_name)){
    print("WEATHER FILES ALREADY EXIST")
    weather_file  <- read.table(weather_file_name, header=TRUE, sep=";")
    weather_shots_file <- read.table(weather_shots_file_name, header=TRUE, sep=";")
    events_we_have <- unique(weather_file$course)
}



# get relevant files
shotlink.directory <- "./data/shotlink"
allFiles <- list.files(shotlink.directory)
pattern <- paste0("^shot-ext-([a-z]*)-", season, "-(\\d+).txt$")
relevant.files <- allFiles[grepl(pattern, allFiles)]
print(relevant.files)

# get relevant events
relevant.events <- events[events$season == season, ]

shot_weathers <- by(relevant.events, 1:nrow(relevant.events), function(event){
    
    # have we already matched info for this event?
    skip_event <- event[["course"]] %in% events_we_have 
    
    # can we find a file for this
    this_tourney.pattern <-  paste0("^shot-ext-([a-z]*)-", season, "-", event[["course"]], ".txt$")
    print(this_tourney.pattern)
    this_tourney.file <- allFiles[which(grepl(this_tourney.pattern, allFiles))]
    this_tourney.file_path <- paste0(shotlink.directory, "/" , this_tourney.file)
    print(paste("tournament file", this_tourney.file, this_tourney.file_path))
    
    event_file_exists <- (file.exists(this_tourney.file_path) & this_tourney.file_path != "./data/shotlink/")
    skip_event <- skip_event | !event_file_exists
    print(event)
    print(paste("skipping", skip_event))

    
    if(!skip_event){
       
        
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
take_by <- lapply(shot_weathers, typeof) == "list"

# bind together
all.shot.weather <- do.call("rbind", shot_weathers[take_by])
all.weather <- format_for_mark(all.shot.weather)


#if we already have info bind what we just got to teh old stuff
if(exists('weather_file')){
    all.shot.weather <- rbind(weather_shots_file, all.shot.weather)
    all.weather <- rbind(weather_file, all.weather)
}

# save everything
write.table(all.shot.weather, weather_shots_file_name, sep = ";", row.names = FALSE)
write.table(all.weather ,weather_file_name,  sep = ";", row.names = FALSE)



