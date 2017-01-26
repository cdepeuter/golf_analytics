# GET ALL COURSES IN ALL FILES

# Find coordinates for unique course


courses.directory <- "./data/event_course_date/"
allFiles <- list.files(courses.directory)
datatables <- list()
for(f in seq_along(allFiles)){
    file <- allFiles[f]
    # only check for txt files
    if(length(grep(".txt", file)) > 0){
        debug.print(file)
        
        newdf <- read.table(paste0(courses.directory,file), sep=";", header=TRUE, stringsAsFactors = FALSE)
        debug.print(dim(newdf))
        datatables[[f]] <- newdf
    }
}


tournaments <- do.call("rbind", datatables)
tournaments$course.1 <- unlist(lapply(tournaments$course.1,trimws))

all.courses <- unique(tournaments$course.1)

courses.coordinates <- unlist(lapply(all.courses, getLocationForPGACourse))