# Load the dplyr and tidyverse libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the CSV file into a data frame
df <- read.csv("./data/global_data.csv")

# Create a data frame with country-continent mapping
country_continent <- data.frame(
  country = c(
    "ARGENTINA", "BRAZIL", "CANADA", "CHILE", "CHINA", "FINLAND", "FRANCE", "GERMANY",
    "GREECE", "HUNGARY", "ISRAEL", "ITALY", "KENYA", "NETHERLANDS", "NORWAY", "POLAND",
    "PORTUGAL", "SPAIN", "TUNISIA", "TURKEY", "URUGUAY", "USA"
  ),
  continent = c(
    "South America", "South America", "North America", "South America", "Asia", "Europe", "Europe", "Europe",
    "Europe", "Europe", "Asia", "Europe", "Africa", "Europe", "Europe", "Europe",
    "Europe", "Europe", "Africa", "Asia", "South America", "North America"
  ),
  color = c(
    "#000000", "#410101", "#efff10", "#600000", "#1fa000", "#8ca000", "#e30000", "#e28000",
    "#e26000", "#e24000", "#8cb000", "#e18000", "#ffdb22", "#e16000", "#e14000", "#e10000",
    "#e08000", "#eea000", "#fffbaa", "#8c9000", "#800000", "#ffff00"
  )
)


# Create the summary data frame with distances as columns and one entry per country
summary_df <- df %>%
  group_by(country, q4_nearby_distance) %>%
  summarize(replies = n()) %>%
  pivot_wider(names_from = q4_nearby_distance, values_from = replies, values_fill = 0)

# Manually reorder the columns
summary_df <- summary_df %>%
  select(country, `1`, `2`, `3`, `4`, `5`, `6`, `7`)

# Calculate the cumulative totals for each distance per country
cumulative_df <- summary_df
cumulative_df[, -1] <- t(apply(cumulative_df[, -1], 1, cumsum))

# Reshape the data to have a single variable for distance class and another for cumulative replies
stacked_data <- pivot_longer(cumulative_df, cols = -country, names_to = "distance_class", values_to = "cumulative_replies")

# Manually reorder the levels of distance_class
stacked_data$distance_class <- factor(stacked_data$distance_class, levels = c("1", "2", "3", "4", "5", "6", "7"))

# Merge the continent information into stacked_data
stacked_data <- stacked_data %>%
  left_join(country_continent, by = "country")

# Combine continent and country names into a new column
stacked_data$legend_label <- paste(stacked_data$continent, stacked_data$country, sep = ": ")

# Sort the countries by continent
stacked_data <- stacked_data %>%
  arrange(continent, country)

# Create a custom color scale by continent
color_scale <- scale_fill_manual(
  values = unique(stacked_data$color),
  breaks = unique(stacked_data$legend_label),
  labels = unique(stacked_data$legend_label)
)

# Define distance class labels
distance_labels <- c(
  "0-400 m", "400-800 m", "800-1200 m", "1200-1600 m",
  "1600-2400 m", "2400-5000 m", "5000-10000 m"
)

# Plotting stacked bar chart for each distance class
stacked_bar_plot <- ggplot(data = stacked_data, aes(x = distance_class, y = cumulative_replies, fill = legend_label)) +
  geom_bar(stat = "identity") +
  color_scale +
  labs(title = "Stacked Bar Chart - Cumulative Replies per Distance Class and Country by Continent",
       x = "Distance Class",
       y = "Cumulative Replies") +
  scale_x_discrete(breaks = 1:7, labels = distance_labels) +  # Rename the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(stacked_bar_plot)
