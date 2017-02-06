
main_scrape_proviz <- function(){
    pga <- getPGAEvents()
    
    pgalocation <- lapply(pga$course.1, getLocationForPGACourse)
    locTable <- do.call(rbind, pgalocation)
    
    pga.allcourses <- unique(pga$course.1)
    dfs <- lapply(pga.allcourses, getCleanProvizForCourse)
    df <- do.call("rbind", dfs)
    
    return(df)
}
