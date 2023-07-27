# Analysis of proximity/distance by country

library(dplyr)
library(data.table)
library(ggplot2)
library(ggstatsplot)
library(ggsci)
library(purrr)
library(tidyr)

# read data
df <- fread(input = './data/global_data.csv')


# 1. reshape data --------------------------------------------------------------

# select columns to use
vars_to_keep <- c('country', 'id', names(df)[names(df) %like% 'q4'] )
df2 <- select(df, all_of(vars_to_keep))

head(df2)

distance_codes <- c(1:7)
distance_values <- c(400,800,1200,1600,2400,5000,10000)

code_df <- data.frame(distance_codes,distance_values)

# Add the distances to df2

df3 <- merge(df2, code_df, by.x = "q4_nearby_distance", by.y = "distance_codes", all.x = TRUE)

# 2. summary statistics -----------------------------------------------------


# Calculate the summary statistics by country
summary_stats <- df3 %>%
  dplyr::group_by(country) %>%   ##this is hard pointing to this package because it gets corrupted by another
  dplyr::summarize(weighted_mean_distance = weighted.mean(distance_values),
                   median_distance = median(distance_values),
                   min_distance = min(distance_values),
                   max_distance = max(distance_values),
                   total_professionals = n()
  )

# Calculate the weighted means for each country from the summary_stats dataframe
weighted_means <- summary_stats %>%
  arrange(desc(weighted_mean_distance))  # Sort the summary_stats dataframe by weighted_mean_distance


# 3. Box plots --------------------------------------------------------------

# Convert the "country" column to a factor and set the levels based on the weighted mean distance
df3$country <- factor(df3$country,
                      levels = weighted_means$country[order(weighted_means$weighted_mean_distance, decreasing = TRUE)])

# Create the weighted box plot using ggplot
plot1 <- ggplot(df3, aes(x = country, y = distance_values)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Reasonable Distance for Everyday Activities by Country (Sorted by Average Distance)",
       y = "Distance",
       x = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. ECDF plots ------------------------------------------------------------------

# Create a table with hit counts for each country and distance
hit_counts_table <- df3 %>%
  group_by(country, distance_values) %>%
  summarize(hit_count = n()) %>%
  pivot_wider(names_from = distance_values, values_from = hit_count, values_fill = 0)

# Create a new table for cumulative hits and make it a data frame
cumulative_hits_table <- hit_counts_table
cumulative_hits_table <- as.data.frame(cumulative_hits_table)

# Loop through each row (country) and calculate cumulative hits
for (i in 1:nrow(cumulative_hits_table)) {
  for (j in 3:ncol(cumulative_hits_table)) {
    cumulative_hits_table[i, j] <- as.double(cumulative_hits_table[i, j] + cumulative_hits_table[i, j - 1])
  }
}

# Loop through each row (country) and calculate relative cumulative value because cumsum did not work
for (i in 1:nrow(cumulative_hits_table)) {
  for (j in 2:ncol(cumulative_hits_table)) {
    cumulative_hits_table[i, j] <- cumulative_hits_table[i, j] / cumulative_hits_table[i, ncol(cumulative_hits_table)]
  }
}

# Function to calculate AUC by multiplying distance width and cumulative percentage
calculate_auc <- function(cumulative_percentages) {
  # Calculate the width of each known class (distance)
  class_widths <- c(400, 400, 400, 400, 2600, 5000)

  # Multiply distance width by cumulative percentage at each step
  auc_value <- sum(class_widths * cumulative_percentages[-length(cumulative_percentages)])

  return(auc_value)
}

# Calculate AUC for each country
cumulative_auc_table <- cumulative_hits_table %>%
  mutate(auc_value = apply(.[, -1], 1, calculate_auc))

# Calculate the shape factor, the number 5000 is derived from the area under the straight diagonal
cumulative_auc_table$shape_factor <- (cumulative_auc_table$auc_value - 5000) / 5000


# Convert cumulative_hits_table to long format for plotting
cumulative_hits_long <- cumulative_hits_table %>%
  pivot_longer(cols = -country, names_to = "distance", values_to = "cumulative_percentage")

# Merge cumulative_hits_long with cumulative_auc_table to include the shape factor
cumulative_plot_data <- merge(cumulative_hits_long, cumulative_auc_table, by = "country")

# Plot the ECDF for each country using ggplot2 and facet_wrap
ecdf_plot <- ggplot(cumulative_plot_data, aes(x = as.numeric(distance), y = cumulative_percentage)) +
  geom_step() +
  labs(title = "Cumulative Distance Distribution by Country",
       y = "Cumulative Proportion",
       x = "Distance") +
  facet_wrap(~country, ncol = 3, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add shape factor text to each plot
  geom_text(aes(x = Inf, y = 0.2, label = sprintf("%.2f", shape_factor)),
            hjust = 1.5, vjust = 0, size = 8)

# 5. Create files and print stuff  --------------------------------------------

print(hit_counts_table, n=22)
print(cumulative_hits_table)
print(cumulative_auc_table)
print(plot1)
print(ecdf_plot)
