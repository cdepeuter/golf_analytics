
# 
# getWindScore <- function(obs){
#     meanWind
#     var <- var(obs$)
# }

getLocalTZ <- function(event){
    googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'
    url <- paste0("https://maps.googleapis.com/maps/api/timezone/json?location=", gsub(" ","", paste(event[["hole_lat"]], event[["hole_lon"]], sep=",")), "&timestamp=1&key=", googleKey)
    #print(url)
    resp <- jsonlite::fromJSON(getUrlResponse(url))
    return(resp$timeZoneId)
}



getHourlyDriveDist <- function(shots){
    shots$date <- as.Date(shots$date_time)
    
    drives <- shots[shots$shot_num == 1 & shots$par > 3 & shots$loc_end == 4,]
    hourly_drive_distance <- drives %>% group_by(hour, date) %>% summarise(dist = mean(shot_dis..yards.))
    hourly_drive_distance$datetime <-  as.POSIXct(paste0(hourly_drive_distance$date," ", hourly_drive_distance$hour, ":00"), format = "%Y-%m-%d %H:%M")
    hourly_drive_distance$dist_norm <- hourly_drive_distance$dist / max(hourly_drive_distance$dist)
    hourly_drive_distance <- hourly_drive_distance[order(hourly_drive_distance$date),]
    
    return(hourly_drive_distance)
}


getShotDegrees <- function(shot){
    
    deltax <- as.numeric(shot[["end_x_yards"]]) - as.numeric(shot[["start_x_yards"]])
    deltay <- as.numeric(shot[["end_y_yards"]]) - as.numeric(shot[["start_y_yards"]])
    
    
    return(getAngle(deltax, deltay))
}

getAimDegrees <- function(shot){
    if(shot[["shot_num"]] == 1){
        deltax <- as.numeric(shot[["med_x_yards"]]) - as.numeric(shot[["start_x_yards"]])
        deltay <- as.numeric(shot[["med_y_yards"]]) - as.numeric(shot[["start_y_yards"]])
    }else{
        deltax <- as.numeric(shot[["hole_x_yards"]]) - as.numeric(shot[["start_x_yards"]])
        deltay <- as.numeric(shot[["hole_y_yards"]]) - as.numeric(shot[["start_y_yards"]])
    }
    
    #print(deltax)
    #print(deltay)
    return(getAngle(deltax, deltay))
}


getAngle <- function(delta_x, delta_y){
    #print(delta_x)
    #print(delta_y)
    
    
    val = 90 - atan2(delta_y, delta_x) * 180/pi
    
    #print( atan2(delta_y, delta_x)* 180/pi)
    
    if(val < 0){
        val = 360 + val
    }
    return(val)
}



fixTime <- function(tm){
    if(nchar(tm) == 1){
        return(paste0("00:0", tm))
    }
    if(nchar(tm) == 2){
        return(paste0("00:", tm))
    }
    if(nchar(tm) == 3){
        return(paste0(substr(tm, 1, 1), ":", substr(tm, 2, 3)))
    }
    return(paste0(substr(tm, 1, 2), ":", substr(tm, 3, 4)))
}


getWeatherBeforeShot <- function(shot, observations){
    # for a given shot, find the weather observation with the closest time
    # assuming the observations are at the right zip code and date
    # input: shot in shotlink format, observations in weatherUnderground format
    # output: rain, wind data
    
    
    whichObs <- which.min(abs(as.numeric(observations[["date_time"]] - shot[["date_time"]])))
    closestObservation <- observations[1:whichObs,] 
    
    return(closestObservation)
}


avgDistByRound <- function(shots, group_var = "player"){
    # group by hole and grouping var
    avg_dist <- shots %>% group_by_(.dots = list(group_var, "round")) %>% summarise(avg_dist = mean(shot_dis..yards.), wind_shot_diff = mean(wind_target_angle_diff))
    
    # combine rows for individaual group vars
    casted <- dcast(avg_dist, paste(group_var, "~ round"), value.var = "avg_dist")
    casted_wind <- dcast(avg_dist, paste(group_var, "~ round"), value.var = "wind_shot_diff")
    
    casted <- cbind(casted, casted_wind)
    colnames(casted) <- c(group_var, "r1_dist", "r2_dist", "r3_dist", "r4_dist", group_var, "r1_wind_diff", "r2_wind_diff", "r3_wind_diff", "r4_wind_diff")
    # if by player add the name
    
    if(group_var == "player"){
        #print(casted$player)
        casted$player_name <- id_to_name[as.character(casted$player)]
    }
    
    return(casted)
}

filterShots <- function(shots_n_weather, filter_type = "drives", holes = seq(1,18), rounds = seq(1,5)){
    # right now just get drives on fairway, 
    # TODO use ... to make this a general filtering function
    
    filtered <- shots_n_weather[shots_n_weather$shot_num == 1 & shots_n_weather$par > 3 & shots_n_weather$loc_end == 4,]
    filtered <- filtered[filtered$hole %in% holes,]
    filtered <- filtered[filtered$round %in% rounds,]
    return(filtered)
}


loadAndBindShotsForEvents <- function(events){
    shotlink.directory <- "./data/shotlink"
    allFiles <- list.files(shotlink.directory)
    
    by_shots_each <- by(events, 1:nrow(events), function(event){
        
        print(event)
        #print(paste(typeof(event[["local_tz"]]), typeof(event[["tourn"]]), typeof(event[["perm_tourn"]])))
        
        this_tourney.pattern <-  paste0("^shot-ext-([a-z]*)-", event[["season"]], "-", event[["course"]], ".txt$")
        print(this_tourney.pattern)
        this_tourney.file <- allFiles[which(grepl(this_tourney.pattern, allFiles))]
        
        
        this_tourney.file_path <- paste0(shotlink.directory, "/" , this_tourney.file)
        print(paste("tournament file", this_tourney.file_path))
        
        if(!file.exists(this_tourney.file_path) | this_tourney.file_path == "./data/shotlink/"){
            print(paste("NOO FILLEE", event))
        } else{
            # get this tournament shots

            this_tourney.shots <- getShotlinkExtTable(this_tourney.file, event[["local_tz"]])
          
            
            # format for mark
            return(this_tourney.shots)
        }
    })
    
    
    # make sure you're taking just the data frames
    take_by <- lapply(by_shots_each, typeof) == "list"
    
    # bind together
    all.shots <- do.call("rbind", by_shots_each[take_by])
    
    return(all.shots)
}

