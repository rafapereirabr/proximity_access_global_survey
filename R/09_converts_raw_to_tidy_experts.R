library(dplyr)
library(data.table)
library(readxl)
library(janitor)
library(stringr)

# read raw global data from excel file
df <- readxl::read_xlsx('data_raw/Results EXPERTS Survey_for phase2 paper.xlsx', skip = 0)

# convert to data.table
data.table::setDT(df)

# clean names - remove empty spaces, all lowercase etc etc
df <- janitor::clean_names(df)
head(df)

# add question number to answer columns
names(df)[3:16] <- paste0('q1_', names(df)[3:16])
names(df)[18:34] <- paste0('q5_', names(df)[18:34])
names(df)[36:52] <- paste0('q6_', names(df)[36:52])
names(df)[54] <- "q4_threshold"
names(df)[55] <- "country"

# turn all quantitative entries into numbers

df <- df %>%
  mutate_at(vars(3:54), ~as.numeric(str_extract(., "\\d+")))

# save into csv

# save data
data.table::fwrite(df, file = './data/expert_data.csv')

