#read fry data

library(tidyverse)
fry_data <- read.csv("../../data/fry_data_all.csv",header=T)

#changing date from string to datetime
fry_data$date <- as.POSIXlt(fry_data$date)

#summing by date
fry_data_sum <- fry_data %>%
  group_by(date) %>%
  summarize(total_catch = sum(total_catch), time_minutes = sum(soak_time_minutes))

#making new column for catch per hour and year

fry_data_sum$catch_per_hour <- fry_data_sum$total_catch*60/fry_data_sum$time_minutes

fry_data_sum$year <- year(fry_data_sum$date)

write.csv(fry_data_sum,"../../data/fry_data_sum.csv")

