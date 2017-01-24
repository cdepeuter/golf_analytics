# load pga data
source("./main_loadPGAData.R")

proviz.files <-lapply(pga$course.1, getProvizForCourse)
proviz.df <-data.frame(matrix(mapply(c, pga$course.1, proviz.files), byrow=T, nrow=length(proviz.files)))
colnames(proviz.df) <- c("course", "file", "score")
write.table(proviz.df, "./data/proviz_courses_2016.csv", sep=",", row.names = FALSE)