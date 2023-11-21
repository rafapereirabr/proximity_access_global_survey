# We make figure3 which contains bar charts for experts vs. global practitioners

library(ggplot2)
library(tidyr)
library(dplyr)

# Read in raw data for both sets

df_global_raw <- read.csv("data/global_data.csv")
df_expert_raw <- read.csv("data/expert_data.csv")

# Create two data frames with the same headers

df_expert <- df_expert_raw[54]
df_expert$type <- "Expert"
names(df_expert)[1] <- "distance_thresholds"

df_global <- df_global_raw[22]


# Since the df_global only contains distance classes, we have to replace values

distance_classes <- c(1,2,3,4,5,6,7)
distance_thresholds <- c(400,800,1200,1600,2400,5000,10000)
dist_conv <- data.frame(distance_classes,distance_thresholds)

df_global <- inner_join(df_global,dist_conv, by=c("q4_nearby_distance"="distance_classes"))
df_global$type <- "Practitioner"
df_global <- df_global[2:3]

# Now both dataframes have a similar format so we can bind them

df_combined <- rbind(df_expert, df_global)
df_combined$row <- "All"

# Let's try some plotting
# For now I took the greyscale, and forced the order of bars. Reversed the legend.

figure_3_plot_stackedbar <- ggplot(df_combined, aes(x=factor(type,level=c("Practitioner", "Expert")), fill=factor(distance_thresholds))) +
  geom_bar(position = position_fill(reverse=TRUE)) +
  theme_bw() +
  scale_fill_grey(guide=guide_legend(reverse=TRUE))+
  labs(y="Proportion of repsonses", x="Type of respondent")

print(figure_3_plot_stackedbar)

figure_3_plot_pie <- ggplot() +
  geom_bar(data=df_combined, aes(x="", fill=factor(distance_thresholds)), position = position_fill())+
  coord_polar(theta="y", direction=-1)+
  facet_grid(.~ factor(type, level=c("Practitioner","Expert")))+
  theme_bw()+
  scale_fill_grey()+
  labs(y="", x="", fill="distance class upper limit (m)")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor =element_blank())


print(figure_3_plot_pie)

