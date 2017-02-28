args <- commandArgs()

setwd("~/code/golfAnalysis")
#library(golfAnalysis, lib.loc=file.path("~/code/golfAnalysis"))

print("doin it")
#print(args)
tourn_id <- as.integer(args[6])
year <- as.integer(args[7])
print(tourn_id)
print(year)

events <- getPGAEvents()

event <- events[events$perm_tourn == tourn_id & events$season == season,]

print(event)




