shotlink.big <- read.csv("./data/shot-2016-pga.txt", sep=";", header=TRUE)
daly <- shotlink.big[shotlink.big$Player.. == 1249,]
