#' Get Shotlink tables
#'
#' These functions load a table from the correct directory
#' @param dataframe evnts
#' @keywords pga golf
#' @return Data Frame of shots
#' @export
#' @import chron
#' @import readr
#' @import reshape2
#' @examples getShotlinkExtTable("shot-ext-safeway-2017.txt", safeway$local_tz)
#' 
#' 


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
    
    # convert all measurments to yards
    tb$shot_dis_yards <- tb$shot_dis_inch / 36
    tb$dis_hole_start_yards <- tb$dis_hole_start_inch / 36
    tb$dis_hole_end_yards <- tb$dis_hole_end_inch / 36
    
    tb$end_x_yards <- tb$end_x_feet / 3
    tb$end_y_yards <- tb$end_y_feet / 3
    tb$end_z_yards <- tb$end_z_feet / 3
    
    # i think these columns are in feet but need to verify
    tb$start_x_yards <- tb$start_x / 3
    tb$start_y_yards <- tb$start_y / 3
    tb$start_z_yards <- tb$start_z / 3
    tb$tee_x_yards <- tb$tee_x / 3
    tb$tee_y_yards <- tb$tee_y / 3
    tb$tee_z_yards <- tb$tee_z / 3
    tb$med_x_yards <- tb$med_x / 3
    tb$med_y_yards <- tb$med_y / 3
    tb$med_z_yards <- tb$med_z / 3
    tb$hole_x_yards <- tb$hole_x / 3
    tb$hole_y_yards <- tb$hole_y / 3
    tb$hole_z_yards <- tb$hole_z / 3
    
    # shot and aim degrees
    tb$shot_degrees <-  unlist(apply(tb, 1, getShotDegrees))
    tb$target_degrees <- unlist(apply(tb, 1, getAimDegrees))
    
    
    # print(colnames(tb))
    # print("cols")
    # only grab yard distance columns + everything else
    columns_to_take <- c("season", "course", "perm_tourn", "hole", "par", "shot_num", "num_strokes", "player", "round","course_name", "tourn_name","player_first", 
                         "player_last" ,"sg_baseline", "hole_score", "seq_tourn", "time", "shot_type", "loc_start", "loc_start_detail", "loc_end", 
                         "loc_end_detail", "in_hole", "recov", "act_year", "act_month", "act_day", "next_drop", "start_baseline", "end_baseline", 
                         "sg_gm", "hour", "date_time", "shot_degrees", "target_degrees", "shot_dis_yards", "dis_hole_start_yards", "dis_hole_end_yards", 
                         "end_x_yards", "end_y_yards", "end_z_yards", "start_x_yards", "start_y_yards", "start_z_yards", "tee_x_yards", "tee_y_yards", "tee_z_yards", 
                         "med_x_yards", "med_y_yards", "med_z_yards", "hole_x_yards", "hole_y_yards", "hole_z_yards")
    
    
    #mask <- !columns_to_take %in% colnames(tb)
    #print(mask)
    #print(colnames(columns_to_take)[mask])
    
    # season;course;perm_tourn;hole;par;shot_num;num_strokes;shot_dis_inch;dis_hole_start_inch;dis_hole_end_inch;player;round;sg_baseline;
    # hole_score;seq_tourn;time;end_x_feet;end_y_feet;end_z_feet;shot_type;loc_start;loc_end;loc_start_detail;loc_end_detail;in_hole;recov;
    # act_year;act_month;act_day;next_drop;start_x;start_y;start_z;tee_x;tee_y;tee_z;hole_x;hole_y;hole_z;med_x;med_y;med_z;green_cntr_x;
    # green_cntr_y;green_cntr_z;hole_quad;miss_side_center;miss_side_hole;green_fall_line;putt_angle;green_slope_deg;start_baseline;
    # end_baseline;sg_gm;course_name;tourn_name;player_first;player_last
    # 
    tb <- tb[, columns_to_take]
    
    return(tb)
}



getDistance <- function(x_start, x_end, y_start, y_end){
    return(sqrt((x_start - x_end)**2 + (y_start - y_end)**2))
}



#' Get Shotlink tables
#'
#' These functions load a table from the correct directory
#' @param dataframe evnts
#' @keywords pga golf
#' @return Data Frame of shots
#' @export
#' @import chron
#' @import readr
#' @import reshape2
#' @examples getShotlinkTable("shot-ext-safeway-2017.txt", safeway$local_tz)
#' 
#' 

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

