



main_scrape_proviz <- function(){
    pga <- getPGAEvents()
    
    pgalocation <- lapply(pga$course.1, getLocationForPGACourse)
    locTable <- do.call(rbind, pgalocation)
    
    pga.allcourses <- unique(pga$course.1)
    pga.ids <- unlist(lapply(pga.allcourses, getCourseId, pga))
    print(pga.ids)
    print(length(pga.ids))
    dfs <- lapply(pga.allcourses, getCleanProvizForCourse)
    df <- do.call("rbind", dfs)
    df <- cbind(df, pga.ids)
    return(df)
}


getCourseId <- function( courseName, events){
    id <- events[events[["course.1"]] == courseName, "course"]
    print(courseName)
    print(id)
    return(id[[1]])
}

add_id_to_proviz <- function(){
    proviz <- read.csv("./data/pga-hole-coords.csv", header=TRUE)
    event_course_id <- events
}

