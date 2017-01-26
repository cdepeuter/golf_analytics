# load pga data

source("./scrape/events.R")


all <- getPGAEvents()
courses <- unique(all$course.1)
proviz.files <-lapply(courses, getProvizForCourse)
proviz.df <-data.frame(matrix(mapply(c, courses, proviz.files), byrow=T, nrow=length(proviz.files)))
colnames(proviz.df) <- c("course", "file", "score")
write.table(proviz.df, "./data/proviz_courses_2016.csv", sep=",", row.names = FALSE)