#bmw championship had some rain, what does it look like
#source scrapeEvents.R and scrapeWeather.R before this

#for the functions that make requests to websites sometimes need to run multiple times, maybe because i'm on a bus


bmw.info <- events[events$id == 2504,]
bmw.addr <- getEventAddressESPN(2504)


#actually grabbing weather info for start day
bmw.resp <- getWeatherResponseForAddrDate(bmw.addr, bmw.info$start)


#write data to file
write(bmw.resp, paste("./json/", bmw.info$id, "_", bmw.info$start, ".json", sep=""))

#get weather info
bmw.weather <- getDataFromWeatherResp(bmw.resp)

#response looks like this
# mean Wind      rain  min temp  max temp 
# "9"    "0.49"      "69"      "78" 



#get Players Championship info
players.info <- events[events$id == 2492,]
players.addr <- getEventAddressESPN(2492)
players.resp <- getWeatherResponseForAddrDate(players.addr, players.info$start)
players.weather <- getDataFromWeatherResp(players.resp)

write(players.resp, paste("./json/", players.info$id, "_", players.info$start, ".json", sep=""))
#response
# mean Wind      rain  min temp  max temp 
# "4"    "0.00"      "64"      "87" 
