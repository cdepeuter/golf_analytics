print("getting package from github")
#library(devtools)
#library(httr)
#with_config(use_proxy(url = "198.169.4.24", port = 3128), install_github("cdepeuter/golf_analytics"))
#install_github("cdepeuter/golf_analytics")
#library(golfAnalysis)


events <- getPGAEvents()
events <- getLocationForEvents(events)
args <- commandArgs()
season <- as.integer(args[6])
#season <- 2017

print(paste("getting files for season", season))
# do we already have a fule for these events?
weather_shots_file_name <- paste0("data/shots_complete-weather-ext-", season, ".txt")
weather_file_name <- paste0("data/shot-ext-weather-", season, ".txt")
events_we_have <- c()

if(file.exists(weather_file_name)){
    print(paste("File", weather_file_name, "exists. Only writing new tournaments"))
    weather_file  <- read.table(weather_file_name, header=TRUE, sep=";")
    weather_shots_file <- read.table(weather_shots_file_name, header=TRUE, sep=";")
    events_we_have <- unique(weather_file$course)
}else{
    print(paste("File", weather_file_name, "does not exist, writing from scratch"))
}



# get relevant files
shotlink.directory <- "./data/shotlink"
allFiles <- list.files(shotlink.directory)
pattern <- paste0("^shot-ext-([a-z]*)-", season, "-(\\d+).txt$")
relevant.files <- allFiles[grepl(pattern, allFiles)]

print("Files for current season")
print(relevant.files)
# get relevant events
relevant.events <- events[events$season == season, ]

shot_weathers <- by(relevant.events, 1:nrow(relevant.events), function(event){
    
    # have we already matched info for this event?
    skip_event <- event[["course"]] %in% events_we_have

    print(event)
    print(paste("skipping", skip_event))

    
    if(!skip_event){
        this_tourney.pattern <-  paste0("^shot-ext-([a-z]*)-", season, "-", event[["course"]], ".txt$")
        this_tourney.file <- allFiles[which(grepl(this_tourney.pattern, allFiles))]
        
        
        this_tourney.file_path <- paste0(shotlink.directory, "/" , this_tourney.file)
        print(paste("tournament file", this_tourney.file, this_tourney.file_path))
        
        if(!file.exists(this_tourney.file_path) | this_tourney.file_path == "./data/shotlink/"){
            print("NOO FILLEE")
        } else{
            # get this tournament shots
            this_tourney.shots <- getShotlinkExtTable(this_tourney.file, event[["local_tz"]])
            
            # get this tournament weather
            this_tourney.weather <- getWeatherObsLocationDates(event[,c("hole_lat", "hole_lon")], event[,c("start", "end")])
            
            print(paste("matching shots and weather for ", event[["tourn"]]))
            # match shots and weather
            start.time  <- Sys.time()
            this_tourney.shot_weather <- matchWeatherToShots(this_tourney.shots, this_tourney.weather)
            end.time <- Sys.time()
            
            
            print(paste("time taken", as.character(end.time - start.time)))
            # get tournament summary
            
            # format for mark
            return(this_tourney.shot_weather)
        }
    }
})


# make sure you're taking just the data frames
take_by <- lapply(shot_weathers, typeof) == "list"



# bind together, if we have stuff to add
if(sum(take_by) > 0){
    all.shot.weather <- do.call("rbind", shot_weathers[take_by])
    all.weather <- format_for_mark(all.shot.weather)
    
}

print(paste("writing files", weather_shots_file_name, weather_file_name))
#if we already have info bind what we just got to teh old stuff

if(file.exists(weather_file_name)){
    all.shot.weather <- rbind(weather_shots_file, all.shot.weather)
    all.weather <- rbind(weather_file, all.weather)
} else{
    all.shot.weather <- weather_shots_file
    all.weather <- weather_file
}

# generate yearly report of daya
yearlyReport(all.shot.weather)

# save everything
write.table(all.shot.weather, weather_shots_file_name, sep = ";", row.names = FALSE)
write.table(all.weather, weather_file_name, sep=";", row.names = FALSE)