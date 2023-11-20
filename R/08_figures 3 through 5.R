
# First we make figure3 which contains bar charts for experts vs. global practitioners

library(ggplot2)
library(tidyr)
library(dplyr)

# Read in raw data for both sets

df_global_raw <- read.csv("data/global_data.csv")
df_expert_raw <- read.csv("data/expert_data.csv")

# Create two data frames with the same headers

df_expert <- df_expert_raw[3:16]
df_global <- df_global_raw[5:18]

# The expert data has to be converted to the same format as the global data
# For this we use Cecilia's idea to count 4 or higher as yes (1), otherwise no (0)


df_expert[df_expert < 4] <- 0
df_expert[df_expert > 3] <- 1

# add column for experts and practitioners

df_expert$type <- "Expert"
df_global$type <- "Practitioner"

# Now we can join both data frames

df_combined <- rbind(df_expert, df_global)

# Reshape the data for easier plotting
df_plot <- df_combined %>%
  gather(variable, value, -type) %>%
  group_by(type, variable) %>%
  summarize(total_score = sum(value))

# Identify the order of levels based on the total scores of practitioners
order_levels <- df_plot %>%
  filter(type == "Practitioner") %>%
  arrange(total_score) %>%
  pull(variable)

# Calculate percentages of totals for each group
df_plot <- df_plot %>%
  group_by(type) %>%
  mutate(percentage = total_score / sum(total_score) * 100)

# Sort the levels of the "variable" factor based on the total scores of practitioners in reverse order
df_plot <- df_plot %>%
  mutate(variable = factor(variable, levels = order_levels))

# Format the labels


# Create horizontal bar charts
plot_fig3_v1 <- ggplot(df_plot, aes(x = percentage, y = variable, fill = type)) +
  geom_bar(stat = "identity", position = "identity", aes(x = ifelse(type == "Expert", -percentage, percentage)), width = 0.7) +
  labs(title = "Experts vs. Global Practitioners",
       x = "Percentage",
       y = "Terminology") +
  theme_minimal() +
  facet_grid(. ~ type, scales = "free_y", space = "free_x") +
  scale_x_discrete(labels = function(y) gsub("q1 ", "", gsub("_", " ", x)))


print(plot_fig3_v1)

# Create horizontal bar charts
plot_fig3_v2 <- ggplot(df_plot, aes(x = percentage, y = variable, fill = type)) +
  geom_bar(stat = "identity", position = "identity", aes(x = percentage), width = 0.7) +
  labs(title = "Experts vs. Global Practitioners",
       x = "Percentage",
       y = "Terminology") +
  theme_minimal() +
  facet_grid(. ~ type, scales = "free_y", space = "free_x") +
  scale_y_discrete(labels = function(x) gsub("q1 ", "", gsub("_", " ", x)))


print(plot_fig3_v2)
