# Making figure 5: continents

# stacked bars by city size from the practitioners data

# We make figure4 which city size threshold evaluations for global practitioners

library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

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

countries <- c(   "ALL",
                  "KENYA", "TUNISIA",
                  "CHINA",
                  "USA", "CANADA",
                  "ARGENTINA", "BRAZIL", "CHILE", "URUGUAY",
                  "FINLAND", "GERMANY", "NETHERLANDS", "POLAND", "NORWAY",
                  "GREECE", "HUNGARY", "TURKEY", "ISRAEL",
                  "FRANCE", "ITALY", "PORTUGAL", "SPAIN")
continents <- c(0,
                1,1,
                2,
                3,3,
                4,4,4,4,
                5,5,5,5,5,
                6,6,6,6,
                7,7,7,7)

cont_conv <- data.frame(countries,continents)

df_global <- inner_join(df_global,cont_conv, by=c("country"="countries"))
df_global$firstbar <- "0" # To get the right order

# Calculating the cutoff value to order the continents

summarydata_dist <- df_global %>% count(continents,distance_thresholds, name="count")
summarydata_dist <- summarydata_dist %>% group_by(continents) %>% mutate(cum_sum = cumsum(count))
summarydata_cont <- df_global %>% count(continents)
summarydata_dist <- inner_join(summarydata_dist, summarydata_cont, by = "continents")
summarydata_dist$cumrel <- summarydata_dist$cum_sum/summarydata_dist$n
summarydata_cutoff <- filter(summarydata_dist, distance_thresholds %in% "1200")


df_global <- inner_join(df_global, select(summarydata_cutoff, continents, cumrel), by= "continents")

# A table where the continents get the sample size

cont.numbers <- 0:7
cont.labs <- c(sprintf("All, n=%d", length(df_global$firstbar)),
                          sprintf("Africa, n=%d", summarydata_cont[1,2]),
                          sprintf("Asia, n=%d", summarydata_cont[2,2]),
                          sprintf("N America, n=%d", summarydata_cont[3,2]),
                          sprintf("S America, n=%d", summarydata_cont[4,2]),
                          sprintf("N Europe, n=%d", summarydata_cont[5,2]),
                          sprintf("SE Europe & ME, n=%d", summarydata_cont[6,2]),
                          sprintf("S Europe, n=%d", summarydata_cont[7,2])
                          )

df_cont.labs <- data.frame(cont.numbers, cont.labs)

df_global <- inner_join(df_global, df_cont.labs, by=c("continents"="cont.numbers"))

# Now, the data needs to be sorted

summarydata_cutoff <- inner_join(summarydata_cutoff,df_cont.labs, by=c("continents"="cont.numbers"))
summarydata_cutoff <- summarydata_cutoff[order(summarydata_cutoff$cumrel),]

# We now renumber the continents from 1 to 7

summarydata_cutoff$continents2 <- 1:7

# And reassign these new numbers to the df_global table

df_global <- merge(df_global, select(summarydata_cutoff, continents, continents2),
                by.x = "continents",
                by.y = "continents",
                all.x = TRUE,
                sort = FALSE)

# Also reassign these new numbers to the df_cont.labs dataframe for the labels

df_cont.labs <- merge(df_cont.labs, select(summarydata_cutoff, continents, continents2),
                      by.x = "cont.numbers",
                      by.y = "continents",
                      all.x = TRUE,
                      sort = FALSE)

df_cont.labs$continents2[df_cont.labs$cont.numbers == 0] <- 0 # The "All" label

df_cont.labs <- df_cont.labs[order(df_cont.labs$continents2),] # Now they are in order of the 1200 meter cumulative percentile


# The plotting begins

plot_5_stacked_bars <- ggplot(data=df_global, x=fct_reorder(cont.labs,factor(cumrel)))+
  geom_bar(data=df_global, aes(x=factor(firstbar), fill=factor(distance_thresholds)), position=position_fill(reverse=TRUE), width = 0.9)+
  geom_bar(data=df_global, aes(x=factor(continents2), fill=factor(distance_thresholds)), position=position_fill(reverse=TRUE), width = 0.5)+
  theme_bw() +
  scale_x_discrete(labels=c("0" = df_cont.labs[1,2],
                            "1" = df_cont.labs[2,2],
                            "2" = df_cont.labs[3,2],
                            "3" = df_cont.labs[4,2],
                            "4" = df_cont.labs[5,2],
                            "5" = df_cont.labs[6,2],
                            "6" = df_cont.labs[7,2],
                            "7" = df_cont.labs[8,2]))+
  scale_fill_grey(guide=guide_legend(reverse=TRUE))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(y="Proportion of repsonses", x="Continent", fill="distance class upper limit")



print(plot_5_stacked_bars)
