#' The correspondig pro-viz file for a given course
#'
#' This function finds the corresponding proviz course file in the data/proviz/ directory
#' There is a text file, exceptions.txt which manually corrects some errors
#' @param string input name of course
#' @return string of file in /data/proviz directory for the corresponding file name.
#' @export
#' @examples 
#' @import RecordLinkage


getCourseForProvizFile <- function(file){
    
    all.courses <- getPGAEvents()
    candidates <- unique(all.courses$course.1)
    candidates.scores <-  unlist(lapply(candidates ,function(x, file){
        file <- gsub("proviz-", "", file)
        c <- gsub(" ", "", x)
        c <- gsub("[(]|[)]", "", c)
        
        return(stringdist(x, file, method = "jw"))
        
    }, file))
    
    return(trimws(candidates[which.min(candidates.scores)]))
}

#' Get Proviz in the cleaned format 
#'
#' This function returns a data frame of events for the pga tour between specific years, or at specific courses/tournaments
#' @param event event returned
#' @keywords pga golf
#' @return Data Frame of events for the given year range.
#' @export
#' @examples getEventsForYears(2010, 2015)
#' 
#' 



getCleanProvizForCourse <- function(courseName){
    course <- paste0(getProvizFileForCourse(courseName), ".php")
    
    course.url <- paste0("http://www.provisualizer.com/courses/", course)
    print(course.url)
    course.html <- getUrlResponse(course.url)
    course.id <- getIDFromResp(course.html)
    if(! is.na(course.id)){
        course.3dUrl <- getCourse3dUrl(paste0("https://www.provisualizer.com/3dlink.php?id=", course.id))
        print(course.3dUrl)
        course.points <- getHolePointsForCourse(course.3dUrl)
        course.points$link <- course.url
        course.points$course <- courseName
        course.points$course_id
        #re-arrange cols
        frontNames <- c("course", "link")
        course.points <- course.points[, c(frontNames, colnames(course.points)[!colnames(course.points) %in% frontNames])]
        return(course.points)
    } else{
        print(paste("na course id, skipping", courseName))
    }
    
    
}


#' The correspondig pro-viz file for a given course
#'
#' This function finds the corresponding proviz course file in the data/proviz/ directory
#' There is a text file, exceptions.txt which manually corrects some errors
#' @param string input name of course
#' @return string of file in /data/proviz directory for the corresponding file name.
#' @export
#' @examples


getProvizFileForCourse <- function(course){
    
    fileName <- "./data/proviz/exceptions.txt"
    candidateFiles <- "./data/proviz/"
    if(!file.exists(fileName)){
        #maybe this is running a test, check other wd
        fileName <- "../../data/proviz/exceptions.txt"
        candidateFiles <- "../../data/proviz/"
    }
    #exceptions
    exceptions <-  read.table(fileName, sep=",", stringsAsFactors = FALSE, row.names = 1 )
    
    thisException <- exceptions[course, "V2"]
    if(!is.na(thisException)){
        return(trimws(thisException))
    }
    
    candidates <- list.files(candidateFiles)
    #print(candidates)
    candidates.scores <-  unlist(lapply(candidates ,function(x, course){
        x <- gsub("proviz-", "", x)
        c <- gsub(" ", "", course)
        c <- gsub("[(]|[)]", "", c)
        
        #return(stringdist(x, c, method = "jw"))
        return(levenshteinSim(x, c))
        
    }, course))
    
    
    winner <- trimws(candidates[which.max(candidates.scores)])
    winner <- gsub("proviz-", "", winner)
    winner <- gsub(".csv", "", winner)
    
    return(winner)
}


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
    courseData <- proviz %>% group_by( type, hole, numPoint) %>% summarise(lat = if(latlong[1]=="Lat"){coords[1]}, lon = if(latlong[2]=="Lon"){coords[2]})
    courseData <- courseData[order(as.integer(courseData$hole)),]
    
    #first, get elevation for all points
    elevationString <- paste(courseData$lat, courseData$lon, sep=",", collapse="|")
    courseData$elevation <- getElevationAtPoints(elevationString)
    
    holeData <- courseData %>% group_by(hole) %>% summarise(
            hole_lat = lat[which(type == "Pin")],
            hole_lon = lon[which(type == "Pin")],
            hole_z = elevation[which(type == "Pin")],
            tee_lat = lat[which(type == "Tee")],
            tee_lon = lon[which(type == "Tee")],
            tee_z = elevation[which(type == "Tee")],
            midpt1_lat = lat[which(type == "Target")][1],
            midpt1_lon = lon[which(type == "Target")][1],
            midpt1_z = elevation[which(type == "Target")][1],
            midpt2_lat = lat[which(type == "Target")][2],
            midpt2_lon = lon[which(type == "Target")][2],
            midpt2_z = elevation[which(type == "Target")][2]
    )
    
    holeData <- holeData[order(as.integer(holeData$hole)),]
    

    return(data.frame(holeData))
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
            print(course.url)
            course.html <- getUrlResponse(course.url)
            course.id <- getIDFromResp(course.html)
            if(! is.na(course.id)){
                course.3dUrl <- getCourse3dUrl(paste0("https://www.provisualizer.com/3dlink.php?id=", course.id))
                print(course.3dUrl)
                course.points <- getHolePointsForCourse(course.3dUrl)
                course.points$link <- course.url
                
                print(paste("printing course points to file", courseFile))
                write.table(course.points, courseFile, sep = ",", row.names = FALSE) 
            } else{
                print(paste("na course id, skipping", courseName))
            }
            
            
        }else{
            print(paste("not scraping ", courseName, ", grabbing locally"))
        }
    })
}


