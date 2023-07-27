# Analysis of proximity/distance by country

library(dplyr)
library(data.table)
library(ggplot2)
library(ggstatsplot)
library(ggsci)
library(purrr)


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

# Create a data frame with all distance values from 0 to 10000 meters for each country
full_data <- expand.grid(country = levels(df3$country),
                         distance_values = seq(0, 10000, by = 100))

# Compute the ECDF for each country
ecdf_data <- full_data %>%
  group_by(country) %>%
  mutate(ecdf_values = ecdf(df3$distance_values[df3$country == country])(distance_values),
         ecdf_percent = ecdf_values * 100)  # Scale to percentages

# Calculate the AUC for each country
auc_values <- ecdf_data %>%
  split(.$country) %>%
  map_dbl(~ with(., sum(diff(distance_values) * ecdf_percent[-1])))

# Calculate the AUC of the triangle (above the diagonal line from (0,0) to (10000,100))
triangle_area <- 0.5 * 10000 * 100

# Compare the AUC of each country to the AUC of the triangle and normalize
normalized_auc <- (auc_values - triangle_area) / triangle_area

# Create a data frame with the results
comparison_result <- data.frame(country = levels(df3$country),
                                Normalized_AUC = normalized_auc)

# Convert the "country" column to a factor and set the levels based on the normalized AUC
comparison_result$country <- factor(comparison_result$country,
                                    levels = comparison_result$country[order(comparison_result$Normalized_AUC, decreasing = TRUE)])

# Print the comparison result with normalized AUC
print(comparison_result)

# Create plots per country using ggplot with fill below ECDF curve
plot2 <- ggplot(ecdf_data, aes(x = distance_values, y = ecdf_percent, fill = country)) +
  geom_step() +
  geom_ribbon(aes(ymax = ecdf_percent), ymin = 0, alpha = 0.2) +
  labs(title = "Cumulative Distance Distribution by Country",
       y = "Cumulative Proportion",
       x = "Distance (Percent of Responses)") +
  facet_wrap(~country, ncol = 6, scales = "free_y") +  # Set custom y-axis limit for each plot and increase ncol
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())   # Hide x-axis labels and ticks

print (plot1)
print (plot2)
