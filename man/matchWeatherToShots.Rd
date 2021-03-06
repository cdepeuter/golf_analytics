% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shotWeatherSummary.R
\name{matchWeatherToShots}
\alias{matchWeatherToShots}
\title{#' get_cumulative_rain
#'
#' This function turns the rain observations which are overlapping 
#' @param date_time shot_date_time input first year of events to return, Defaults to 0 (no lower bound)
#' @param data.frame weather last year of events to return, Defaults to 10000 (no upper bound)
#' @param boolean for_mark format data for mark or no
#' @keywords pga golf
#' @return Data Frame row of summary weather info for the shot
#' @export
#' @examples shotWeatherSummary(safeway.shots[1,], safeway.weather)
#' @import httr
#' @import dplyr
#'}
\usage{
matchWeatherToShots(shots, weather)
}
\arguments{
\item{shots}{data.frame input first year of events to return, Defaults to 0 (no lower bound)}

\item{data.frame}{weather all weather obs for the tournament}
}
\value{
Data Frame  summary weather info for each shot
}
\details{
get_cumulative_rain <- function(obs_time, weather){
    hours_to_look <- c(1, 2, 4, 6, 12, 18, 24, 36, 48)
    
    weather_time_diff <- weather$date_time - obs_time
    units(weather_time_diff) <- "mins"
    
    
    rain_before_obs <- lapply(hours_to_look, function(x){
        return(sum(weather[which(weather_time_diff > - x * 60 & weather_time_diff <= 0), "precip"], na.rm = TRUE))
    })
    
    
    rain_df <- data.frame(as.list(rain_before_obs))
    colnames(rain_df) <-  paste0("rain_",  hours_to_look, "_hrs_before")
    
    return(rain_df)
}
matchWeatherToShots

This function takes all shots in a tournament, and all weather observations we have for the tournament
and returns a dataframe with relevant weather info for each shot
}
\examples{
matchWeatherToShots(safeway.shots, safeway.weather)
}

