setwd("C:/Users/user/Desktop/STAT_525/Final_Project/cleaned_data")
file_path1 <- "data_cleaned_2023.csv"
data_2023 <- read.csv(file_path1)
file_path2 <- "data_cleaned_2024.csv"
data_2024 <- read.csv(file_path2)
data <- rbind(data_2023, data_2024)
data <- data[data$Offense_group == "A", ]
data$time_index <- paste(data$Year, "/",data$Month, sep = "")
table(data$time_index)

# Y ~ pois(l)
# l ~ gamma(a, b)
# l|Y ~ gamma(a + Y, b + 1)

a <- 200
b <- 0.1


classes <- unique(data$Beat_class)

posterior_samples <- list()

# Loop through each class in Beat_class
for (class in classes) {
  y <- table(data[data$Beat_class == class, "Month"])
  
  update_a <- a + sum(y)  # Sum of observations for the class
  update_b <- b + length(y)  # Number of observations (months) for the class
  
  posterior_samples[[class]] <- rgamma(1000, update_a, update_b)
}

plot(NULL, xlim = c(500, 2800), ylim = c(0, 0.06), 
     xlab = "Lambda (Rate)", ylab = "Density", 
     main = "Posterior Densities for Each Beat_class")
colors <- rainbow(length(classes))  # Assign colors for each class

x <- seq(1500, 2500, length = 2000)
lines(x, dgamma(x, shape = a, rate = b), type = "l", lty = "solid",
      col = "blue", lwd = 2, ylab = "Density", xlab = "Value", 
      main = "Overlay of Gamma PDF and Posterior Samples")

for (i in seq_along(classes)) {
  lines(density(posterior_samples[[classes[i]]]), col = colors[i], lwd = 2)
  }

legend("topright", legend = classes, col = colors, lwd = 2, title = "Beat_class")


hist(posterior_samples[[classes[1]]], freq = FALSE)
plot(density(posterior_samples[[classes[1]]]), xlim = c(1000, 2500))
classes[1]

means <- lapply(posterior_samples, mean)
quantiles <- lapply(posterior_samples, function(x) quantile(x, probs = c(0.025, 0.975)))

result_df <- data.frame(
  Beat_class = names(means),                              # Extract names as a column
  Mean = unlist(means), 
  lower_bound = as.vector(sapply(quantiles, function(q) q[1])),
  upper_bound = as.vector(sapply(quantiles, function(q) q[2])),
  Credible_Interval = sapply(quantiles, function(x)       # Combine quantiles into "(lower, upper)" format
    sprintf("(%.3f, %.3f)", x[1], x[2])))

population_data <- data.frame(
  Beat_class = c("Central", "North", "Northeast", "Eastside", "South Central", 
                 "Clear Lake", "Southeast", "South Gessner", "Southwest", "Westside", "Northwest"),
  Population = c(2.03623, 2.58808, 2.03450, 0.91272, 0.96108, 1.55328, 
                 1.76931, 1.54044, 2.17443, 3.40857, 1.56922)
)

# Add the population column to the result_df by merging
final_df <- merge(result_df, population_data, by = "Beat_class")
final_df$criminal_rate <- final_df$Mean / final_df$Population
final_df$criminal_rate_interval <- mapply(
  function(lower, upper) {
    sprintf("(%.2f, %.2f)", lower, upper)
  },
  lower = final_df$lower_bound/(final_df$Population),
  upper = final_df$upper_bound/(final_df$Population)
)
colnames(final_df)
final_output <- final_df[, c("Beat_class", "Mean",  
                                  "Credible_Interval", 
                                  "criminal_rate", "criminal_rate_interval")]
write.csv(final_output, "groupA_table.csv")

plot(NULL, xlim = c(1000, 2500), ylim = c(0, 0.06), 
     xlab = "Lambda (Rate)", ylab = "Density", 
     main = "Posterior Densities for Each Beat_class")
colors <- rainbow(length(classes))  # Assign colors for each class

x <- seq(1000, 2500, length = 2000)
for (i in seq_along(classes)) {
  lines(density(posterior_samples[[classes[i]]]/final_df[final_df$Beat_class == classes[i], "Population"]), col = colors[i], lwd = 2)
}


# Create a data frame for plotting
plot_data <- data.frame()
i <- 1
posterior_samples[[classes[i]]]/final_df[final_df$Beat_class == classes[i], "Population"]
# Loop through classes and calculate densities
for (i in seq_along(classes)) {
  density_data <- density(posterior_samples[[classes[i]]] / 
                            final_df[final_df$Beat_class == classes[i], "Population"])
  plot_data <- rbind(plot_data, 
                     data.frame(Lambda = density_data$x, 
                                Density = density_data$y, 
                                Beat_class = classes[i]))
}

# ggplot2 plot
library("ggplot2")
ggplot(plot_data, aes(x = Lambda, y = Density, color = Beat_class)) +
  geom_line(size = 1.2) +  # Add density lines
  scale_color_manual(values = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    "#bcbd22", "#17becf", "#aec7e8"
  )) +  # Assign colors
  labs(title = "Monthly Criminal Rate Distribution per 100,000 Population",
       x = "Monthly Criminal Rate",
       y = "Density",
       color = "Beat Class") +  # Add legend title
  theme_minimal() +
  theme(legend.position = "right")

heatmap_df <- final_df[, c("Beat_class", "criminal_rate")]