#' Get pga events 
#'
#' This function returns a data frame of events for the pga tour between specific years, or at specific courses/tournaments
#' @param integer input first year of events to return, Defaults to 0 (no lower bound)
#' @param integer input last year of events to return, Defaults to 10000 (no upper bound)
#' @param string course Name of course to filter by
#' @param string tournament Name of tournament to filter by
#' @keywords pga golf
#' @return Data Frame of events for the given year range.
#' @export
#' @examples getEventsForYears(2010, 2015)
#' 
#' 


getPGAEvents <- function(startYear=0, endYear=10000, course = NA, tournament = NA) {
    
    # if (!requireNamespace("pkg", quietly = TRUE)) {
    #     stop("Pkg needed for this function to work. Please install it.",
    #          call. = FALSE)
    # }
    courses.directory <- "./data/event_course_date/"

    
    allFiles <- list.files(courses.directory)
    if(length(allFiles) ==  0 ){
        #maybe this is running a test, check other wd
        courses.directory <- "../../data/event_course_date/"
        allFiles <-  list.files(courses.directory)
        print(length(allFiles))
    }
    datatables <- list()
    for(f in seq_along(allFiles)){
        file <- allFiles[f]
        # only check for txt files
        
        tgp <- grep(".txt", file)

        if(length(tgp) > 0){
            
            
            reg <- 'event_course_date-(\\d)+'
            extracted <- regexpr(reg, file)
            yearStr <- substr(file,extracted+18,extracted+attr(extracted,"match.length")-1)
            
            year <- as.integer(yearStr)
            if(year >= startYear & year <= endYear){
                newdf <- read.table(paste0(courses.directory,file), sep=";", header=TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"), quote="")
                
                newdf <- newdf[,colSums(is.na(newdf))<nrow(newdf)]
                completeCases <- apply(newdf, 1, function(x) all(is.na(x)))
                newdf <- newdf[ !completeCases, ]
                
                datatables[[f]] <- newdf
                
            }
        }
    }
    
    
    tournaments <- do.call("rbind", datatables)
    
    # clean strings
    tournaments$course.1 <- unlist(lapply(tournaments$course.1,trimws))
    tournaments$tourn <- unlist(lapply(tournaments$tourn,trimws))
    
    # clean dates
    tournaments$start <- as.Date(paste(tournaments$min_year, tournaments$min_month, tournaments$min_day, sep="/"), format="%Y/%m/%d")
    tournaments$end <- as.Date(paste(tournaments$max_year, tournaments$max_month, tournaments$max_day, sep="/"), format="%Y/%m/%d")
    tournaments <- tournaments[, !(colnames(tournaments) %in% c("min_month", "max_month", "min_year", "max_year", "min_day", "max_day", "X" ))]
    
    
    #filter by course/tournament
    if(! is.na(course)){
        tournaments <- tournaments[tournaments$course.1 == course,]
    }
    
    if(! is.na(tournament)){
        tournaments <- tournaments[tournaments$tourn == tournament,]
    }

    
    return(tournaments)
}
