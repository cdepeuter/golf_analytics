
print("getting package from github")
library(devtools)
library(httr)
with_config(use_proxy(url = "198.169.4.24", port = 3128), install_github("cdepeuter/golf_analytics"))
#install_github("cdepeuter/golf_analytics")
library(golfAnalysis)

print(paste("getting weather for season", season, "course", course))
args <- commandArgs()
season <- as.integer(args[6])
course <- as.integer(args[7])

thisEvent <- events[(events$season == season) & (events$course) == course,]
print("event found:")
print(thisEvent)


historicalWeatherForEvent(thisEvent)
