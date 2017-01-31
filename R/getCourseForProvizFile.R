#' The correspondig pro-viz file for a given course
#'
#' This function finds the corresponding proviz course file in the data/proviz/ directory
#' There is a text file, exceptions.txt which manually corrects some errors
#' @param string input name of course
#' @return string of file in /data/proviz directory for the corresponding file name.
#' @export
#' @examples
#' getProvizFileForCourse("TPC Scottsdale")


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