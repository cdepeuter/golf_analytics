
getShotlinkTable <- function(filename){
    tb <- read.csv(paste0("./data/shotlink/", filename),  sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    # remmove last column and row
    tb <- tb[, colSums(is.na(tb)) < nrow(tb)]
    
    time <- unlist(lapply(tb$Time, fixTime))
    tb$time <- time
    date <- tb$Date
    date_time <- as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M")
    
    # TODO not standardied
    tb$hour <- hours(date_time)-4
    tb$date_time <- date_time
    
    return(tb)
}


getShotlinkExtTable <- function(filename, timezone = NULL){
    tb <- read.csv(paste0("./data/shotlink/", filename),  sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    # remmove last column and row
    tb <- tb[, colSums(is.na(tb)) < nrow(tb)]
    tb <- tb[1:nrow(tb)-1,]
    
    if(is.null(timezone)){
        warning("GIVE A TIMEZONE")
    }
    
    time <- unlist(lapply(tb$time, fixTime))
    tb$time <- time
    
    date <- paste(tb$act_month, tb$act_day, tb$act_year, sep="/")
    tb$Date <- as.Date(date, format = "%m/%d/%Y")
    
    
    date_time <- as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M", tz=timezone)
    
    # TODO not standardied
    tb$hour <- hours(date_time)-4
    tb$date_time <- date_time
    
    tb$shot_degrees <-  unlist(apply(tb, 1, getShotDegrees))
    tb$aim_degrees <- unlist(apply(tb, 1, getAimDegrees))
    
    return(tb)
}