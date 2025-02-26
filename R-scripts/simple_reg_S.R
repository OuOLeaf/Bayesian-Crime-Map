setwd("C:/Users/user/Desktop/STAT_525/Final_Project/cleaned_data")
file_path1 <- "data_cleaned_2023.csv"
data_2023 <- read.csv(file_path1)
file_path2 <- "data_cleaned_2024.csv"
data_2024 <- read.csv(file_path2)
data <- rbind(data_2023, data_2024)
data$time_index <- paste(data$Year, "/",data$Month, sep = "")
data$hour <- data$RMSOccurrenceHour
data$period <- ifelse(data$hour <= 6 | data$hour >= 18, 1, 0)

gibbs_sampler <- function(Y, t, n_iter, mu_beta0, sigma_beta0, mu_beta1, sigma_beta1, alpha_sigma2, beta_sigma2) {
  # Number of observations
  n <- length(Y)
  
  # Initialize parameters
  beta0 <- 0
  beta1 <- 0
  sigma2 <- 1
  samples <- matrix(NA, nrow = n_iter, ncol = 3)
  colnames(samples) <- c("beta0", "beta1", "sigma2")
  
  for (iter in 1:n_iter) {
    # Step 1: Sample beta0
    sigma2_beta0 <- 1 / (n / sigma2 + 1 / sigma_beta0^2)
    mu_beta0_post <- sigma2_beta0 * (sum(Y - beta1 * t) / sigma2 + mu_beta0 / sigma_beta0^2)
    beta0 <- rnorm(1, mean = mu_beta0_post, sd = sqrt(sigma2_beta0))
  
    
    # Step 2: Sample beta1
    sigma2_beta1 <- 1 / (sum(t^2) / sigma2 + 1 / sigma_beta1^2)
    mu_beta1_post <- sigma2_beta1 * (sum((Y - beta0) * t) / sigma2 + mu_beta1 / sigma_beta1^2)
    beta1 <- rnorm(1, mean = mu_beta1_post, sd = sqrt(sigma2_beta1))

    # Step 3: Sample sigma2
    alpha_post <- alpha_sigma2 + n / 2
    beta_post <- beta_sigma2 + sum((Y - beta0 - beta1 * t)^2) / 2
    sigma2 <- 1 / rgamma(1, shape = alpha_post, rate = beta_post)
    # Store samples
    samples[iter, ] <- c(beta0, beta1, sigma2)
  }
  
  return(samples)
}




set.seed(123)
beat_classes <- unique(data$Beat_class)
prior_data = read.csv("prior_information.csv")

posterior_list = list()

for(beat in beat_classes){
  print(beat)
  pdata = data[data$Beat_class == beat & data$Offense_group == "A", ]
  Y <- as.vector(table(pdata$period, pdata$time_index)) 
  t <- rep(c(0,1), length(Y)/2)
  
  mu_beta0 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][2])
  mu_beta1 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][3])
  
  
  sigma_beta0 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][4])
  sigma_beta1 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][5])
  
  alpha_sigma2 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][6])
  beta_sigma2 <- as.numeric(prior_data[prior_data$Beat_class == beat, ][7])
  
  n_iter <- 11000
  samples <- gibbs_sampler(Y, t, n_iter, mu_beta0, sigma_beta0, mu_beta1, sigma_beta1, alpha_sigma2, beta_sigma2)
  
  burn_in <- 1000
  posterior_samples <- samples[(burn_in + 1):n_iter, ]
  
  posterior_list[[beat]] <- posterior_samples
}

library(ggplot2)
library(dplyr)
library(tidyr)

# Add an iteration column for plotting the trace
south_central_df <- as.data.frame(posterior_list[["South Central"]]) %>%
  mutate(iteration = 1:nrow(.)) %>%  # Assign unique iterations
  pivot_longer(cols = -iteration, names_to = "parameter", values_to = "value")  # Reshape data

# Create the trace plots with facet_wrap for a 1-row layout
ggplot(south_central_df, aes(x = iteration, y = value)) +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y", 
             labeller = as_labeller(c(
               "beta0" = expression(beta[0]),
               "beta1" = expression(beta[1]),
               "sigma2" = expression(sigma^2)
             ))) +
  labs(x = "Iteration", y = "Value", title = "Trace Plots for South Central Parameters") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))


# Initialize an empty data frame
result_df <- data.frame(
  beat = character(),
  beat_mean = numeric(),
  p_beta1_less_than_0 = numeric()
)

# Loop through each beat and calculate p(beta1 < 0)
for (beat in beat_classes) {
  beat_mean <- mean(posterior_list[[beat]][, "beta1"])
  # Calculate the probability that beta1 is less than 0
  p_beta1_less_than_0 <- mean(posterior_list[[beat]][, "beta1"] < 0)
  
  # Add the result to the data frame
  result_df <- rbind(result_df, data.frame(beat = beat, beat_mean = beat_mean, p_beta1_less_than_0 = p_beta1_less_than_0))
}
write.csv(result_df, "beta_1.csv", row.names = FALSE)
beta1_data <- do.call(rbind, lapply(beat_classes, function(beat) {
  data.frame(
    beta1 = posterior_list[[beat]][, "beta1"],
    beat = beat
  )
}))

# Plot the distribution for each beat
ggplot(beta1_data, aes(x = beta1, color = beat, fill = beat)) +
  geom_density(alpha = 0.3) +  # Density plot with transparency
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at x = 0
  labs(
    title = "Posterior Distributions of Beta_1",
    x = expression(beta[1]),
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title