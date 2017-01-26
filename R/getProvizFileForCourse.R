#' The correspondig pro-viz file for a given course
#'
#' This function finds the corresponding proviz course file in the data/proviz/ directory
#' There is a text file, exceptions.txt which manually corrects some errors
#' @param string input name of course
#' @return string of file in /data/proviz directory for the corresponding file name.
#' @export
#' @examples
#' getProvizFileForCourse("TPC Scottsdale")


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
    candidates.scores <-  unlist(lapply(candidates ,function(x, course){
        x <- gsub("proviz-", "", x)
        c <- gsub(" ", "", course)
        c <- gsub("[(]|[)]", "", course)
        
        return(levenshteinSim(x, course))
        
    }, course))
    
    return(trimws(candidates[which.max(candidates.scores)]))
}