library(dplyr)
library(data.table)
library(ggplot2)
library(ggstatsplot)
library(ggsci)


# read data
df <- fread(input = './data/global_data.csv')


# 1. reshape data --------------------------------------------------------------

# select columns to use
vars_to_keep <- c('country', 'id', names(df)[names(df) %like% 'q6|q10'] )
df2 <- select(df, vars_to_keep)
df2[, q10_professional_activity_positions := NULL]

head(df2)



# convert data to long format
df3_time <- data.table::melt(df2,
                             id.vars = c('country','id'),
                             measure = patterns("^q6_"),
                             variable.name = 'activity',
                             value.name = 'time')

df3_field <- data.table::melt(df2,
                             id.vars = c('country','id'),
                             measure = patterns("^q10_"),
                             variable.name = 'field',
                             value.name = 'field_dummy')

# merge
df3_time <- na.omit(df3_time)
df3_field <- subset(df3_field, field_dummy == 1)
df3 <- left_join(df3_time, df3_field, by = c('country', 'id'), relationship = "many-to-many")
df3 <- na.omit(df3)
head(df3)




# 2. recode variables ----------------------------------------------------------

# change field labels
table(df3$field)
df3[, field := gsub('q10_', '', field) ]
df3[, field := gsub('_', ' ', variable) ]
df3[, field := fcase(field == 'consultant_responsibilities', 'Consultant',
                     field == 'political_responsibilities_politicians_decision_makers', 'Political',
                     field == 'technical_responsibilities_planners', 'Technical',
                     field == 'other_responsibilities', 'Others')]



# change activity labels
table(df3$activity)
df3[, activity := gsub('q6_', '', activity) ]
df3[, activity := gsub('2', ' ', activity) ]
df3[, activity := gsub('_', ' ', activity) ]


# categorize activities
df3[, activity_cat := fcase(activity %like% 'education|school', 'Education',
                     activity %like% 'medical|hospitals|pharmacies', 'Health',
                     activity %like% 'jobs', 'Jobs',
                     activity %like% 'cultural|parks|playground', 'Leisure',
                     default = "Others")]

# change time labels
df3[, time := fcase(time == 1, 5,
                    time == 2, 10,
                    time == 3, 15,
                    time == 4, 20,
                    time == 5, 30,
                    time == 6, 45,
                    time == 7, 60)]


# 3. figures -------------------------------------------------------------------

# reorder based on total average
df3[, time_avg := mean(time), by = activity]
df3 <- df3[order(-time_avg)]
df3[, activity := factor(activity, levels = unique(activity))]


## 3.0 activities -------------------------------------------------------------------
f0 <-
  ggplot() +
  geom_boxplot(data = df3,
               aes(color = activity_cat, y = time, x =  activity),
               outlier.alpha = .1) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
  labs(x = 'Time in minutes', y = 'Activity', color = 'Category') +
  ggsci::scale_color_jama() +
  coord_flip() +
  theme_classic()

f0
ggsave(f0, file = './figures/4_time_activities.png',
       height = 18, width = 20, units = 'cm')


## 3.1 general -------------------------------------------------------------------
f1 <-
  ggplot() +
  geom_boxplot(data = df3,
           aes(color = field, y = time, x =  field),
           outlier.alpha = .1) +
  facet_wrap(.~activity_cat, ncol = 2) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
  labs(y = 'Time in minutes', x = 'Professional activity') +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(legend.position="none")

f1
ggsave(f1, file = './figures/4_time_fields_general.png',
       height = 14, width = 12, units = 'cm')




## 3.2 health -------------------------------------------------------------------

f2 <- ggplot() +
      geom_boxplot(data = subset(df3, activity_cat == 'Health'),
                   aes(color = field, y = time, x =  field),
                   outlier.alpha = .1) +
      facet_wrap(.~activity, ncol = 3) +
      scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
      labs(y = 'Time in minutes', x = 'Professional activity') +
      ggsci::scale_color_npg() +
      theme_classic() +
      theme(legend.position="none")

f2
ggsave(f2, file = './figures/4_time_fields_health.png',
       height = 8, width = 18, units = 'cm')




## 3.3 Education -------------------------------------------------------------------

f3 <- ggplot() +
  geom_boxplot(data = subset(df3, activity_cat == 'Education'),
               aes(color = field, y = time, x =  field),
               outlier.alpha = .1) +
  facet_wrap(.~activity, ncol = 2) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
  labs(y = 'Time in minutes', x = 'Professional activity') +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(legend.position="none")

f3
ggsave(f3, file = './figures/4_time_fields_education.png',
       height = 16, width = 16, units = 'cm')





## 3.4 Leisure -------------------------------------------------------------------

f4 <- ggplot() +
  geom_boxplot(data = subset(df3, activity_cat == 'Leisure'),
               aes(color = field, y = time, x =  field),
               outlier.alpha = .5) +
  facet_wrap(.~activity, ncol = 3) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
  labs(y = 'Time in minutes', x = 'Professional activity') +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(legend.position="none")

f4
ggsave(f4, file = './figures/4_time_fields_leisure.png',
       height = 8, width = 18, units = 'cm')





## 3.4 Leisure -------------------------------------------------------------------

f5 <- ggplot() +
  geom_boxplot(data = subset(df3, activity_cat == 'Others'),
               aes(color = field, y = time, x =  field),
               outlier.alpha = .5) +
  facet_wrap(.~activity, ncol = 3) +
  scale_y_continuous(breaks = c(0, 20, 40, 60), labels = c('0', '20', '40', '60+')) +
  labs(y = 'Time in minutes', x = 'Professional activity') +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(legend.position="none")

f5
ggsave(f5, file = './figures/4_time_fields_others.png',
       height = 18, width = 18, units = 'cm')

