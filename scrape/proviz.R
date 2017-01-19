source("utils.R")

getCourse3dUrl <- function(url){
    # input: 3dlink url with no parameters
    # output: link to redirected url after 302 response

    resp <- GET(url)
    return(resp$url)
}


getIDFromResp <- function(html){
    # input: html for a proviz course home page
    # output: id for course
    
    reg <- 'href=\\"\\/3dlink.php\\?id=(\\d)+'
    extracted <- regexpr(reg, html)
    idStr <- substr(html,extracted+21,extracted+attr(extracted,"match.length")-1)

    return(as.integer(idStr))
}


getHolePointsForCourse <- function(url){
    # input: url with coordinates appeded as parameters for 3d proviz page
    # output: data frame with points for course
    
    proviz.html <- getUrlResponse(url)

    proviz.jsIX <- regexpr("<script(.*)</script>", proviz.html)
    proviz.js <- substr(proviz.html,proviz.jsIX,proviz.jsIX+attr(proviz.jsIX,"match.length")-1)
    
    proviz.splitArray <- unlist(strsplit(proviz.js, ";|\\n", fixed=FALSE))
    proviz.mask <- unlist(lapply(proviz.splitArray, function(x){return(grepl("setCourseHole",x))}))
    proviz.splitArray <- proviz.splitArray[proviz.mask]
    
    proviz <- tbl_df(do.call("rbind", lapply(proviz.splitArray, parseHolesFromScript )))
    
    # group by hole & pointType get both lat and long as columns
    courseData <- proviz %>% group_by( type, hole, numPoint) %>% summarize(lat = if(latlong[1]=="Lat"){coords[1]}, lon = if(latlong[2]=="Lon"){coords[2]})
    courseData <- courseData[order(as.integer(courseData$hole)),]
    
    #no string
    courseData[["lat"]] <- as.double(courseData[["lat"]])
    courseData[["lon"]] <- as.double(courseData[["lon"]])
    
    return(courseData)
}

parseHolesFromScript <- function(x){
    # input: javascript commands setting latitude and longitude for each hole
    # output: ungrouped array of js command params in tabular form
    
    spl <-strsplit(x, "[(]|[,]|[)]", fixed=FALSE)[[1]]
    
    if(length(spl) == 4){
        spl <-append(spl, 0, after = 3)
    }
    infoText <- spl[1]
    latLong <- substr(infoText, nchar(infoText)-2, nchar(infoText))
    type <- gsub("setCourseHole", "", substr(infoText, 0, nchar(infoText)-3))
    
    spl <- c(type, latLong, spl[2:5])
    names(spl) <- c("type", "latlong", "course", "hole", "numPoint", "coords")
    
    return(spl)
}

getAllProvizCourses <- function(){
    # input: none
    # output: list of urls for all courses available on proviz
    url <- "https://www.provisualizer.com/courses/"
    pg <- read_html(url)
    
    urls <- html_attr(html_nodes(pg, "a"), "href")
    urls <- urls[grepl(".php", urls)]

    return(urls)
}


getCornersOfCourse <- function(points){
    
    #first two center
    return(c(
        mean(points[["lat"]]),
        mean(points[["lon"]]),
        min(points[["lat"]]),
        max(points[["lat"]]),
        min(points[["lon"]]),
        max(points[["lon"]])
    ))
}


main_scrapeProviz <- function(){
    
    proviz.courses <- getAllProvizCourses()
    t <- lapply(proviz.courses, function(course){
        
 
        courseName <- gsub(".php", "", course)
        courseFile <- paste0("./data/proviz/", "proviz-", courseName, ".csv")
        if(!file.exists(courseFile)){
            course.url <- paste0("http://www.provisualizer.com/courses/", course)
            debug.print(course.url)
            course.html <- getUrlResponse(course.url)
            course.id <- getIDFromResp(course.html)
            if(! is.na(course.id)){
                course.3dUrl <- getCourse3dUrl(paste0("https://www.provisualizer.com/3dlink.php?id=", course.id))
                debug.print(course.3dUrl)
                course.points <- getHolePointsForCourse(course.3dUrl)
                debug.print(paste("printing course points to file", courseFile))
                write.table(course.points, courseFile, sep = ",", row.names = FALSE) 
            } else{
                debug.print(paste("na course id, skipping", courseName))
            }
            
            
        }else{
            debug.print(paste("not scraping ", courseName, ", grabbing locally"))
        }
    })
}
