# Load necessary library
library(ggplot2)
library(dplyr)
library(plotly)

setwd("C:/Users/user/Desktop/STAT_525/Final_Project/cleaned_data")
file_path1 <- "data_cleaned_2023.csv"
data_2023 <- read.csv(file_path1)
file_path2 <- "data_cleaned_2024.csv"
data_2024 <- read.csv(file_path2)
data <- rbind(data_2023, data_2024)

NdescriptionA <- table(data[data$Offense_group == "A", ]$NIBRSDescription)
NdescriptionB <- table(data[data$Offense_group == "B", ]$NIBRSDescription)
dfA <- as.data.frame(NdescriptionA)
colnames(dfA) <- c("Description", "Count")
# Compute percentages and cumulative percentages for the pie chart
# Sort the data by Count in descending order
dfA <- dfA %>%
  arrange(desc(Count))
dfA <- dfA %>%
  mutate(
    Description = case_when(
      Description == "Destruction, damage, vandalism" ~ "Damage/Vandalism",
      Description == "Theft from motor vehicle" ~ "Vehicle Theft",
      TRUE ~ Description  # Keep other descriptions unchanged
    )
  )

dfA <- dfA %>%
  mutate(Description = as.character(Description)) %>%  # Ensure Description is text
  arrange(desc(Count))  # Sort by Count in descending order

# Retain the top 10 classes and group the rest as "Others"
dfA <- dfA %>%
  mutate(
    Category = ifelse(row_number() <= 4, Description, "Others")
  ) %>%
  group_by(Category) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(
    Percentage = Count / sum(Count) * 100,  # Compute percentages
    Label = paste0(round(Percentage, 1), "%")  # Create labels
  )


# Create a basic pie chart
plot_ly(dfA, labels = ~Category, values = ~Count, type = "pie", textinfo = "label+percent",
        marker = list(colors = RColorBrewer::brewer.pal(n = nrow(dfA), name = "Set3")))

dfB <- as.data.frame(NdescriptionA)
colnames(dfB) <- c("Description", "Count")
dfB <- dfB %>%
  mutate(
    Description = case_when(
      row_number() == 7 ~ "Trespass Property",
      row_number() == 5 ~ "Driving DUI",
      row_number() == 4 ~ "Disorderly Conduct",
      TRUE ~ "Others"  # All other rows are labeled as "Others"
    )
  ) %>%
  group_by(Description) %>%
  summarise(Count = sum(Count), .groups = "drop")  # Combine counts for "Others"
dfB <- dfB %>%
  mutate(Description = as.character(Description)) %>%  # Ensure Description is text
  arrange(desc(Count))  # Sort by Count in descending order

# Retain the top 10 classes and group the rest as "Others"
dfB <- dfB %>%
  mutate(
    Category = ifelse(row_number() <= 4, Description, "Others")
  ) %>%
  group_by(Category) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(
    Percentage = Count / sum(Count) * 100,  # Compute percentages
    Label = paste0(round(Percentage, 1), "%")  # Create labels
  )

plot_ly(dfB, labels = ~Category, values = ~Count, type = "pie", textinfo = "label+percent",
        marker = list(colors = RColorBrewer::brewer.pal(n = nrow(dfA), name = "Set3")))