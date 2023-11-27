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

df_global$sum_replies <- rowSums(df_global[,2:6])
df_global <- filter(df_global, sum_replies==1)  # If this statement is commented out, the response will be linked to the largest size reported.

# First we add a column, assuming that the largest size class is counted (multiple entries)

df_global$size_class <- case_when(
  df_global[6]==TRUE ~ 1, # Small cities
  df_global[5]==TRUE ~ 2, # Medium cities
  df_global[4]==TRUE ~ 3, # Large cities
  df_global[3]==TRUE ~ 4, # Very large cities
  df_global[2]==TRUE ~ 5  # Megacities
)

# An attempt to create labels in the dataframe, it does not help to force the order of bars.

city_sizes <- 0:5
city_labels <- c(sprintf("All, n=%d", length(df_global$size_class)),
                 sprintf("Small, n=%d", length(which(df_global$size_class==1))),
                 sprintf("Medium, n=%d", length(which(df_global$size_class==2))),
                 sprintf("Large, n=%d", length(which(df_global$size_class==3))),
                 sprintf("Very large, n=%d", length(which(df_global$size_class==4))),
                 sprintf("Mega, n=%d", length(which(df_global$size_class==5)))
                 )
cs_conv <- data.frame(city_sizes, city_labels)

df_global <- inner_join(df_global, cs_conv, by=c("size_class"="city_sizes"))

# Let's try some plotting
# For now I took the greyscale, and forced the order of bars. Reversed the legend


figure_4_plot_stackedbar <- ggplot() +
  geom_bar(data = df_global,
           position = position_fill(reverse=TRUE), # from high to low
           aes(x=factor(sum_replies/sum_replies), # did this to force it to be 1, else it would end up in another stack
               fill=factor(distance_thresholds)
               ),
           width=0.9
           ) +
  geom_bar(data = df_global,
           position = position_fill(reverse=TRUE),
           aes(x=factor(size_class+1),
               fill=factor(distance_thresholds)
               ),
           width=0.5
           ) +
  scale_x_discrete(labels=c("1" = cs_conv[1,2],
                            "2" = cs_conv[2,2],
                            "3" = cs_conv[3,2],
                            "4" = cs_conv[4,2],
                            "5" = cs_conv[5,2],
                            "6" = cs_conv[6,2]))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=20,hjust=1))+
  labs(y="Proportion of repsonses", x="City Size", fill="distance class upper limit")+
  scale_fill_grey(guide=guide_legend(reverse=TRUE))

# I had to use an index to get all bars into one graph in the right order, and added 1 because '1' was already taken by 'all replies'.
# The scale_x_discrete statement forces the labels of the columns.

print(figure_4_plot_stackedbar)
