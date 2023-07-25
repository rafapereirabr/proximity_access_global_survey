library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(xlsx)
library(janitor)
library(forcats)

df <- readxl::read_xlsx('RESULTS_ALL_COUNTRIES_ValideResponses.xlsx', skip = 1)


df <- df0
df <- janitor::clean_names(df)
setDT(df)
head(df)



### id columns
data.table::setnames(df, 'x1', 'country')
data.table::setnames(df, 'x2', 'id')
data.table::setnames(df, 'x3', 'term_positions')


### Q1 block - Term
data.table::setnames(df, "i_dont_use_any_term_to_refer_to_accessibility_by_proximity", 'no_term')
data.table::setnames(df, "other_19", 'other')
names(df)[3:19] <- paste0('q1_', names(df)[3:19])
data.table::setnames(df, "x20", 'q2_other_term')
data.table::setnames(df, "x21", 'q3_term_comment')


### Q2 block - relevance to reach nearby
data.table::setnames(df, 'x22', 'q4_nearby_distance')
names(df)[23:40] <- paste0('q5_', names(df)[23:40])


### Q3 block - reasonable time to access activity
names(df)[41:58] <- paste0('q6_', names(df)[41:58])
data.table::setnames(df, "x59", 'q7_any_additional_activity')
data.table::setnames(df, "x60", 'q8_additional_activity')
data.table::setnames(df, "x61", 'q8_reasonable_time_to_additional_activity')




### Q4 block - professional activity
data.table::setnames(df, 'x62', 'professional_activity_positions')
data.table::setnames(df, 'other_66', 'other_responsibilities')
names(df)[62:66] <- paste0('q10_', names(df)[62:66] )


data.table::setnames(df, 'x67', 'q11_importance_proximity_profession')


### Q5 block - field of activity
data.table::setnames(df, 'x68', 'professional_field_positions')
data.table::setnames(df, 'other_71', 'any_other_professional_field')
data.table::setnames(df, 'x72', 'other_professional_field')
names(df)[68:72] <- paste0('q12_', names(df)[68:72] )


### Q6 block - city size
data.table::setnames(df, 'x73', 'city_size_positions')
names(df)[73:79] <- paste0('q14_', names(df)[73:79] )
data.table::setnames(df, 'q14_x79', 'q15_city')


### Q7 block - demographics
data.table::setnames(df, 'x80', 'age')
data.table::setnames(df, 'x81', 'sex')


# head(df[, 68:72])





#66666666666666666666666666666666666666666666666
# Q1 terms ---------------------------------------------------------------------
df2 <- copy(df)

data.table::setnames(df2, "i_dont_use_any_term_to_refer_to_accessibility_by_proximity", 'no_term')
data.table::setnames(df2, "other_19", 'other1')
data.table::setnames(df2, "x20", 'other2')
# names(df)[3:19] <- paste0('q1_',names(df)[3:19])

# to long format
df2 <- df2[,c(1:17)]


df3 <- data.table::melt(df2 , id.vars = c('country','id'))
head(df3)

df3[, n_country := sum(value>0), by= country]

# proportio of each word in each country
t1 <- df3[, .(prop_term = sum(value) /n_country), by= .(country, variable) ]
t1 <- unique(t1)
t1 <- t1[order(-prop_term, variable)]

# sanity check
t1[, sum(prop_term), by=country]

# order based on total average
t1[, order_avg := mean(prop_term), by = variable]
head(t1)



ggplot(t1, aes(fill = reorder(variable, -order_avg),
               y = prop_term,
               x =  reorder(country, -order_avg)
               )) +
  geom_bar(position="stack", stat="identity")



# Q1 proximity time ---------------------------------------------------------------------

# by pro





library(foreign)

dj <- foreign::read.dta('complete labeled dataset sas format.dta')
dj <- foreign::read.spss('complete labeled dataset spss format.sav')

head(dj)




