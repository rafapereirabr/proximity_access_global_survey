# stacked bars by city size from the practitioners data

# We make figure4 which city size threshold evaluations for global practitioners

library(ggplot2)
library(tidyr)
library(dplyr)

# Read in raw data for both sets

df_global_raw <- read.csv("data/global_data.csv")

df_global <- df_global_raw[c(22,74:78)]


# Since the df_global only contains distance classes, we have to replace values

distance_classes <- c(1,2,3,4,5,6,7)
distance_thresholds <- c(400,800,1200,1600,2400,5000,10000)
dist_conv <- data.frame(distance_classes,distance_thresholds)

df_global <- inner_join(df_global,dist_conv, by=c("q4_nearby_distance"="distance_classes"))

# Define the city size classes and their corresponding labels

# Some surveys have multiple answers, these will be removed.
# If this part is commented out, the response will be linked to the largest size reported.


df_global$sum_replies <- rowSums(df_global[,2:6])
df_global <- filter(df_global, sum_replies==1)

# First we add a column, assuming that the largest size class is counted (multiple entries)

df_global$size_class <- case_when(
  df_global[6]==TRUE ~ 1, # Small cities
  df_global[5]==TRUE ~ 2,  # Medium cities
  df_global[4]==TRUE ~ 3, # Large cities
  df_global[3]==TRUE ~ 4, # Very large cities
  df_global[2]==TRUE ~ 5 # Megacities
)



city_sizes <- 1:5
city_labels <- c("Small Cities (<100,000)",
                 "Medium Cities (100,000-250,000)",
                 "Large Cities (250,000-500,000)",
                 "Very Large Cities (500,000-10,000,000)",
                 "Megacities (>10,000,000)")
cs_conv <- data.frame(city_sizes, city_labels)

df_global <- inner_join(df_global, cs_conv, by=c("size_class"="city_sizes"))

# Let's try some plotting
# For now I took the greyscale, and forced the order of bars. Reversed the legend


figure_4_plot_stackedbar <- ggplot() +
  geom_bar(data = df_global,
           position = position_fill(reverse=TRUE),
           aes(x=factor(sum_replies),
               fill=factor(distance_thresholds)
               )
           ) +

  geom_bar(data = df_global,
           position = position_fill(reverse=TRUE),
           aes(x=factor(size_class+1),
               fill=factor(distance_thresholds)
               )
           ) +
  scale_x_discrete(labels=c("1" = "All", "2" = "Small", "3" = "Medium", "4" = "Large", "5" = "Very large", "6" = "Mega"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(y="Proportion of repsonses", x="City Size", fill="distance class upper limit")+
  scale_fill_grey(guide=guide_legend(reverse=TRUE))


print(figure_4_plot_stackedbar)
