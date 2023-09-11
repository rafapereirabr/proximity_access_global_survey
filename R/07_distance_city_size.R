# Load necessary libraries
library(ggplot2)
library(dplyr)

df <- read.csv("~/Downloads/PhD project/Proximity based accessibility international/CitySize_distance raw.csv", sep=";")
colnames(df) <- c("City size", "Distance", "Count") #just to be sure, sometimes city size becomes City.size which is a problem for the left join.

# 'City size': Categorical variable indicating city size (1..5)
# 'Distance': Categorical variable indicating distance class (1..8)
# 'Count': Number of respondents in each distance class for each city size

# Define the city size classes and their corresponding labels
city_sizes <- 1:5
city_labels <- c("Small Cities (<100,000)",
                 "Medium Cities (100,000-250,000)",
                 "Large Cities (250,000-500,000)",
                 "Very Large Cities (500,000-10,000,000)",
                 "Megacities (>10,000,000)")

# Define the new distance classes (including class 8)
distance_classes <- c(1:8)

# Create a data frame with all combinations of 'City size' and 'Distance'
df_extended <- expand.grid(`City size` = city_sizes, `Distance` = distance_classes)

# Merge 'df_extended' with 'df' to include counts (Count) for each combination
df_extended <- left_join(df_extended, df, by = c("City size", "Distance"))

# Replace missing counts with 0
df_extended[is.na(df_extended$Count), "Count"] <- 0

# Calculate cumulative percentages within each "City size" group
df_extended <- df_extended %>%
  group_by(`City size`) %>%
  mutate(cumulative_count = cumsum(Count),
         total_count = sum(Count),
         cumulative_percentage = cumulative_count / total_count) %>%
  ungroup()

# Define the actual distances in meters corresponding to each distance class
distance_labels <- c(0, 400, 800, 1200, 1600, 2400, 5000, 10000)

# Map the 'Distance' variable to the corresponding distance labels
df_extended$DistanceMeters <- distance_labels[df_extended$Distance]

# Map the 'City size' variable to the corresponding city size labels
df_extended$CitySizeLabels <- case_when(
  df_extended$`City size` == 1 ~ city_labels[1],
  df_extended$`City size` == 2 ~ city_labels[2],
  df_extended$`City size` == 3 ~ city_labels[3],
  df_extended$`City size` == 4 ~ city_labels[4],
  df_extended$`City size` == 5 ~ city_labels[5]
)

# Convert 'CitySizeLabels' to a factor with desired levels and order
df_extended$CitySizeLabels <- factor(df_extended$CitySizeLabels, levels = city_labels)

# Create an ECDF plot for each city size with adjusted x-axis
ecdf_plot <- ggplot(df_extended, aes(x = DistanceMeters, y = cumulative_percentage, group = CitySizeLabels)) +
  geom_step() +
  labs(title = "ECDF for Different City Sizes",
       x = "Distance (meters)",
       y = "Cumulative Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        axis.text.y = element_text(size = 8)) +  # Adjust the text size
  facet_grid(CitySizeLabels ~ .)  # Create a grid of plots with fixed 0 intercept

# Print or display the combined ECDF plot
print(ecdf_plot)

# ...

# Calculate AUC for each city size
auc_values <- df_extended %>%
  group_by(`City size`) %>%
  summarize(auc = sum(diff(DistanceMeters) * cumulative_percentage[-length(cumulative_percentage)])) %>%
  mutate(corrected_auc = (auc - 5000) / 5000)

# Print the AUC values
print(auc_values)

# Print or display the combined ECDF plot
print(ecdf_plot)
