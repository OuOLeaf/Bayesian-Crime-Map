library(sf)
library(ggplot2)
library(dplyr)

# Load GeoJSON Data for Houston Police Districts
districts <- st_read("COH_POLICE_DISTRICTS.geojson")

# Plot the districts map
plot(districts)

# Create a better visualization with ggplot2
ggplot(data = districts) +
  geom_sf(aes(fill = DISTRICT), color = "white") +
  theme_minimal() +
  labs(title = "Houston Police Districts",
       fill = "District") +
  theme(legend.position = "right")

# Save the map
ggsave("districts.png", width = 10, height = 8, dpi = 300)