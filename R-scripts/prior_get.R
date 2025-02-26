setwd("C:/Users/user/Desktop/STAT_525/Final_Project/cleaned_data")
file_path1 <- "data_cleaned_2022.csv"
data_2022 <- read.csv(file_path1)
file_path2 <- "data_cleaned_2021.csv"
data_2021 <- read.csv(file_path2)
data <- rbind(data_2022, data_2021)
data$time_index <- paste(data$Year, "/",data$Month, sep = "")
data$hour <- data$RMSOccurrenceHour
data$period <- ifelse(data$hour <= 6 | data$hour >= 18, 1, 0)
pdata = data[data$Beat_class == "Clear Lake" & data$Offense_group == "A", ]
table(pdata$period, pdata$time_index)

# Y <- as.vector(table(pdata$period, pdata$time_index)) 
# t <- rep(c(0,1), length(Y)/2)
weekly_means <- aggregate(Y ~ t, FUN = mean)  # Mean for each period
day_mean <- weekly_means[weekly_means$t == 0, "Y"]  # Mean for day (period = 0)
night_mean <- weekly_means[weekly_means$t == 1, "Y"]  # Mean for night (period = 1)
# alpha_sigma2 <- 2  # Set weakly informative shape prior
# beta_sigma2 <- var(Y) / 2  # Use half the sample variance as rate

# Unique Beat_classes
beat_classes <- unique(data$Beat_class)

# Initialize a data frame to store prior information
prior_info <- data.frame(
  Beat_class = character(),
  mu_beta0 = numeric(),
  sigma_beta0 = numeric(),
  mu_beta1 = numeric(),
  sigma_beta1 = numeric(),
  alpha_sigma2 = numeric(),
  beta_sigma2 = numeric(),
  stringsAsFactors = FALSE
)

# Loop over Beat_class
for (beat in beat_classes) {
  # Subset data for the current Beat_class and offense group "A"
  pdata <- subset(data, Beat_class == beat & Offense_group == "A")
  
  # Skip if no data available for this Beat_class
  if (nrow(pdata) == 0) {
    next
  }
  
  # Calculate Y and t
  Y <- as.vector(table(pdata$period, pdata$time_index))
  t <- rep(c(0, 1), length(Y) / 2)
  
  # Calculate weekly means and variances
  weekly_means <- aggregate(Y ~ t, FUN = mean)  # Mean for each period
  if (nrow(weekly_means) < 2) {  # Skip if no data for both periods
    next
  }
  day_mean <- weekly_means[weekly_means$t == 0, "Y"]  # Mean for day (period = 0)
  night_mean <- weekly_means[weekly_means$t == 1, "Y"]  # Mean for night (period = 1)
  
  # Calculate priors
  mu_beta0 <- day_mean  # Prior mean for beta0
  sigma_beta0 <- 100  # Prior SD for beta0
  mu_beta1 <- night_mean - day_mean  # Prior mean for beta1
  sigma_beta1 <- 100  # Prior SD for beta1
  alpha_sigma2 <- 2  # Weakly informative prior
  beta_sigma2 <- var(Y) / 2  # Half the variance of Y as rate
  
  # Append to the prior_info data frame
  prior_info <- rbind(prior_info, data.frame(
    Beat_class = beat,
    mu_beta0 = mu_beta0,
    sigma_beta0 = sigma_beta0,
    mu_beta1 = mu_beta1,
    sigma_beta1 = sigma_beta1,
    alpha_sigma2 = alpha_sigma2,
    beta_sigma2 = beta_sigma2
  ))
}

# Save the prior information to a CSV file
output_file <- "prior_information.csv"
write.csv(prior_info, file = output_file, row.names = FALSE)