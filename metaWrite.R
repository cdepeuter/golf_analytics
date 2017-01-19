source('./scrape/weather.R')
metaWeather <- getWeatherForTournaments(events.us)
metaWeatherFile <- paste0("./data/meta_weather_us_", season, ".csv")

write.table(metaWeather, metaWeatherFile, sep=",", row.names = FALSE)