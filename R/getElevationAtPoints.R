#' Elevation for a string of points
#'
#' This function finds elevation for a string of points, separated by |
#' @param string list of points
#' @return vector of elevations in feet
#' @export
#' @import dplyr
#' @examples
#' getElevationAtPoints("40.743484674348,-73.454984245498|40.744527774453,-73.454463145446|40.74670037467,-73.453455145346")



getElevationAtPoints <- function(pointString){

    
    googleKey <- 'AIzaSyDEjTuilt2Ys9HjKf8ZrXzAjvl3d5hhHWg'
    
    elevation.url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", pointString, "&key=", googleKey)
    
    #sometimes a space in elevations, remove from url
    elevation.url <- gsub(" ", "", elevation.url)
    
    elevation.json <- elevation.url %>%
        GET() %>%
        content(as="text") %>%
        jsonlite::fromJSON()
    
    #grab elevation from response json, convert to feet
    elevation <- elevation.json$results$elevation
    return(elevation * 3.28084)
}
