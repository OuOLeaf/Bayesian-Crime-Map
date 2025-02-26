setwd("C:/Users/user/Desktop/STAT_525/Final_Project/cleaned_data")
file_path1 <- "data_cleaned_2023.csv"
data_2023 <- read.csv(file_path1)
file_path2 <- "data_cleaned_2024.csv"
data_2024 <- read.csv(file_path2)
data <- rbind(data_2023, data_2024)
data$time_index <- paste(data$Year, "/",data$Month, sep = "")
table(data$time_index)
data_groupA <- data[data$Offense_group == "A",]
data_groupB <- data[data$Offense_group == "B",]
table(data_groupA$time_index)
table(data_groupB$time_index)