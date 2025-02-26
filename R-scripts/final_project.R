library(readxl)
setwd("C:/Users/user/Desktop/STAT_525/Final_Project")
year_str = "2021"
file_path <- paste("NIBRSPublicView", year_str, ".xlsx", sep = "")
data <- read_excel(file_path)
area_code <- read.csv("area_code.csv")
offense_code <- read_excel("offense_code.xlsx")
x <- data$Beat
y1 <- numeric(length(x))
for(i in 1:nrow(area_code["prefix"])){y1[which(startsWith(x, area_code["prefix"][i,]))] <- unlist(area_code["class"][i,])}
x <- data$NIBRSClass
y2 <- numeric(length(x))
for(i in 1:nrow(offense_code["NIBRSCode"])){y2[which(x == unlist(offense_code["NIBRSCode"][i,]))] <- unlist(offense_code["Group"][i,])}
data["Beat_class"] <- y1
data["Offense_group"] <- y2
data <- data[data$Beat_class != 0, ]
data$Year <- format(data$RMSOccurrenceDate, "%Y")
data$Month <- as.integer(format(data$RMSOccurrenceDate, "%m"))
file_name <- paste("cleaned_data/data_cleaned_", year_str, ".csv", sep = "")
write.csv(data[, c("Year", "Month", "RMSOccurrenceHour", "Beat_class", "NIBRSClass", "NIBRSDescription", "Offense_group")], file = file_name, row.names = FALSE)