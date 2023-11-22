# Making figure 5: contintnents

# stacked bars by city size from the practitioners data

# We make figure4 which city size threshold evaluations for global practitioners

library(ggplot2)
library(tidyr)
library(dplyr)

# Read in raw data for both sets

df_global_raw <- read.csv("data/global_data.csv")

df_global <- df_global_raw[c(1,22)]


# Since the df_global only contains distance classes, we have to replace values

distance_classes <- c(1,2,3,4,5,6,7)
distance_thresholds <- c(400,800,1200,1600,2400,5000,10000)
dist_conv <- data.frame(distance_classes,distance_thresholds)

df_global <- inner_join(df_global,dist_conv, by=c("q4_nearby_distance"="distance_classes"))

#Contextual division of countries

#Africa: Kenya & Tunisia (different contexts but low number of surveys)
#Asia: China
#North America: USA, Canada
#South America: Argentina, Brazil, Chile, Uruguay
#Northern Europe: Finland, Germany, Netherlands, Poland, Norway
#South-East Europe & Middle East: Greece, Hungary, Turkey, Israel
#Southern Europe":France, Italy, Portugal, Spain

countries <- c("KENYA", "TUNISIA",
                  "CHINA",
                  "USA", "CANADA",
                  "ARGENTINA", "BRAZIL", "CHILE", "URUGUAY",
                  "FINLAND", "GERMANY", "NETHERLANDS", "POLAND", "NORWAY",
                  "GREECE", "HUNGARY", "TURKEY", "ISRAEL",
                  "FRANCE", "ITALY", "PORTUGAL", "SPAIN")
continents <- c("1. Africa", "1. Africa",
                   "2. Asia",
                   "3. North America", "3. North America",
                   "4. South America", "4. South America","4. South America","4. South America",
                   "5. Northern Europe","5. Northern Europe","5. Northern Europe","5. Northern Europe","5. Northern Europe",
                   "6. South-East Europe & Middle East", "6. South-East Europe & Middle East","6. South-East Europe & Middle East","6. South-East Europe & Middle East",
                   "7. Southern Europe","7. Southern Europe","7. Southern Europe","7. Southern Europe")

cont_conv <- data.frame(countries,continents)

df_global <- inner_join(df_global,cont_conv, by=c("country"="countries"))
df_global$firstbar <- "0. All"

# The plotting begins

plot_5_stacked_bars <- ggplot()+
  geom_bar(data=df_global, aes(x=factor(firstbar), fill=factor(distance_thresholds)), position=position_fill(reverse=TRUE), width = 0.8)+
  geom_bar(data=df_global, aes(x=factor(continents), fill=factor(distance_thresholds)), position=position_fill(reverse=TRUE), width = 0.3)+
  theme_bw() +
  scale_fill_grey(guide=guide_legend(reverse=TRUE))+
  theme(axis.text.x=element_text(angle=20,hjust=1))+
  labs(y="Proportion of repsonses", x="Continent", fill="distance class upper limit")



print(plot_5_stacked_bars)
