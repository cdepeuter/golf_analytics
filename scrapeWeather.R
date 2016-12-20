#key for weather underground api calls
wugKey <-"8569324e20ad844f"


getWugDateFormat <- function(dateStr){
  #put file in YYYYMMDD format
  
  date <- as.Date(dateStr)
  return(as.character(date, format = "%Y%m%d"))
}

getWeatherResponseForAddrDate <- function(addr, dateStr){
  # get weather info json response for event at given address on date
  # input: info with city, state, zip(maybe)
  # output json response from weather underground api
  
  # DONT run this function in vectorized format on an array
  # weatherUnderground maxes API calls at 10 per minute

  #get date to yyymmdd from 2016-02-04T08:00Z
  wugDateStr <- getWugDateFormat(dateStr)
  
  #if we have the zip code just use that, if not we need to use the town + state
  if(!is.null(addr$zipCode)){
    wugUrl <- paste("http://api.wunderground.com/api/", wugKey,"/history_",wugDateStr, "/q/", addr$zipCode, ".json", sep = "")
  }else if(!is.null(addr$city) && !is.null(addr$state)){
    #take spaces out of city
    city <- addr$city
    city <- gsub(" ", "_", city)
    wugUrl <- paste("http://api.wunderground.com/api/", wugKey,"/history_",wugDateStr, "/q/",  addr$state, "/", city, ".json", sep = "")
  }else{
    return("Not enough info to fetch weather")
  }
  
  weatherReq <- GET(wugUrl)
  weatherContent <- content(weatherReq, as="text")
  return(weatherContent) 
}


getDataFromWeatherResp <- function(weatherContent){
  # get relevant info from weather underground
  # input: json format weather string
  # output: field from response in named list
  
  #put json into table
  weatherJSON <- fromJSON(weatherContent)
  
  dailySummary <- weatherJSON$history$dailysummary
  
  meanWindSpeed <- dailySummary$meanwindspdi
  rain <- dailySummary$precipi
  minTemp <- dailySummary$mintempi
  maxTemp <- dailySummary$maxtempi
  
  #format data to return
  weatherData <- c(meanWindSpeed, rain, minTemp, maxTemp)
  names(weatherData) <- c("mean Wind", "rain", "min temp", "max temp")
  
  return(weatherData)
}


