library(dplyr)
library(data.table)
library(readxl)
library(janitor)

# read raw global data from excel file
df <- readxl::read_xlsx('./data_raw/RESULTS_ALL_COUNTRIES_ValideResponses.xlsx', skip = 1)

# convert to data.table
data.table::setDT(df)

# clean names - remove empty spaces, all lowercase etc etc
df <- janitor::clean_names(df)
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
data.table::setnames(df, "x61", 'q9_reasonable_time_to_additional_activity')



### Q4 block - professional activity
data.table::setnames(df, 'x62', 'professional_activity_positions')
data.table::setnames(df, 'other_66', 'other_responsibilities')
names(df)[62:66] <- paste0('q10_', names(df)[62:66] )


data.table::setnames(df, 'x67', 'q11_importance_proximity_profession')


### Q5 block - field of activity
data.table::setnames(df, 'x68', 'professional_field_positions')
data.table::setnames(df, 'other_71', 'any_other_professional_field')
names(df)[68:71] <- paste0('q12_', names(df)[68:71] )
data.table::setnames(df, 'x72', 'q13_other_professional_field')




### Q6 block - city size
data.table::setnames(df, 'x73', 'city_size_positions')
names(df)[73:78] <- paste0('q14_', names(df)[73:78] )
data.table::setnames(df, 'x79', 'q15_city')



### Q7 block - demographics
data.table::setnames(df, 'x80', 'age')
data.table::setnames(df, 'x81', 'sex')


# head(df[, 68:72])


# save data
data.table::fwrite(df, file = './data/global_data.csv')


