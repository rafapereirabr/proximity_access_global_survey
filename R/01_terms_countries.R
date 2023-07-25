library(dplyr)
library(data.table)
library(ggplot2)
library(forcats)


# read data
df <- fread(input = './data/global_data.csv')

# select columns to use
df2 <- df[,c(1:19)]
df2[, q1_term_positions := NULL]

# convert data to long format
df3 <- data.table::melt(df2 , id.vars = c('country','id'))
head(df3)

# calculate number of answers for each country
df3[, n_country := sum(value>0), by= country]

# proportio of each word in each country
t1 <- df3[, .(prop_term = sum(value) /n_country), by= .(country, variable) ]
t1 <- unique(t1)

# sanity check: it must add to 1 (100%) for each country
t1[, sum(prop_term), by=country]

# change labels
t1[, variable := gsub('q1_', '', variable) ]
t1[, variable := gsub('_', ' ', variable) ]

# reorder based on total average
t1[, order_avg := mean(prop_term), by = variable]
t1 <- t1[order(prop_term, variable)]
t1[, variable := factor(variable, levels = unique(variable))]


# figure
ggplot() +
  geom_bar(data = t1,
           aes(fill = variable,
               y = prop_term,
               x =  country),
           position="stack", stat="identity")




