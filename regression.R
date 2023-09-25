#looking at affect of water temperature and peak spawn time on peak fry emergence
library(tidyverse)
library(lubridate)
library(zoo)

phenomix_peaks <- read.csv("../../data/peaks_phenomix.csv")

spawn <- read.csv("../../data/peak_spawn_dates.csv")

temp <- read.csv("../../data/water_temperature_data_for_model.csv")

#joining different covariates with the phenomix peaks dataframe
peak_w_temp <- left_join(phenomix_peaks, temp, join_by(peak == date))

peak_w_temp_spawn <- left_join(peak_w_temp, spawn, join_by(year == water_year))

#trying some linear models
lm <- lm(value~water_temperature_c, peak_w_temp)
summary(lm)

lm_spawn <- lm(value~yday(peak_escapement), peak_w_temp_spawn)
summary(lm_spawn)

lm_spawn_temp <- lm(value~yday(peak_escapement) + water_temperature_c, peak_w_temp_spawn)
summary(lm_spawn_temp)



#calculate accumulated thermal units (aka atu which is cumulative sum since winter solstice)

#sort increasing order of date
temp <- temp[order(as.Date(temp$date)),]

#since there is NA in temperature column and I want to take cumulative sum,
# I need to change NA to the interpolation of temperature
temp$temp_interpolate <- na.approx(temp$water_temperature_c)

#divide data into different groups starting winter solstice
temp_trial <- temp %>% group_by(date_range = cut(as.Date(date),
                                                 breaks=as.Date(c("2001-12-21",
                                                                  "2002-12-21",
                                                                  "2003-12-21",
                                                                  "2004-12-21",
                                                                  "2005-12-21",
                                                                  "2006-12-21",
                                                                  "2007-12-21",
                                                                  "2008-12-21",
                                                                  "2009-12-21",
                                                                  "2010-12-21",
                                                                  "2011-12-21",
                                                                  "2012-12-21",
                                                                  "2013-12-21",
                                                                  "2014-12-21",
                                                                  "2015-12-21",
                                                                  "2016-12-21",
                                                                  "2017-12-21",
                                                                  "2018-12-21",
                                                                  "2019-12-21",
                                                                  "2020-12-21",
                                                                  "2021-12-21"
                                                                  )),
                                                 labels = FALSE))

#calculate atu
atu <- temp_trial %>% group_by(date_range) %>% mutate(atu = cumsum(temp_interpolate))

#joining these dataframes
peak_w_atu <- left_join(phenomix_peaks, atu, join_by(peak == date))

peak_w_atu_spawn <- left_join(peak_w_atu, spawn, join_by(year == water_year))


lm_spawn_temp_atu <- lm(value~yday(peak_escapement) + water_temperature_c + atu, peak_w_atu_spawn)
summary(lm_spawn_temp_atu)
AIC(lm_spawn_temp_atu) #94

lm_atu <- lm(value~ atu, peak_w_atu_spawn)
summary(lm_atu)
AIC(lm_atu) #94

lm_spawn_atu <- lm(value~yday(peak_escapement) + atu, peak_w_atu_spawn)
summary(lm_spawn_atu)
AIC(lm_spawn_atu) #92

lm_atu_temp <- lm(value~ water_temperature_c + atu, peak_w_atu_spawn)
summary(lm_atu_temp)
AIC(lm_atu_temp) #96


#both spawn date and atu are close to being significant

#save csv

write.csv(peak_w_atu_spawn, "../../data/peaks_phenomix_W_covariates.csv")

#maybe regression should be between peak date and atu at a particular date (maybe April 17, doy = 105)

atu$doy <- yday(atu$date)

atu_subset <- atu[atu$doy == 105,]

atu_subset$year <- atu_subset$brood_year +1

atu_subset <- atu_subset  %>% select(year, atu) %>% rename(atu_105 = atu)


peak_w_atu_spawn_105 <- left_join(peak_w_atu_spawn, atu_subset, join_by(year))



lm_spawn_atu_105 <- lm(value~yday(peak_escapement) + atu_105, peak_w_atu_spawn_105)
summary(lm_spawn_atu_105)
AIC(lm_spawn_atu_105) #85
plot(fitted(lm_spawn_atu_105),residuals(lm_spawn_atu_105))
plot(lm_spawn_atu_105)

ggplot(peak_w_atu_spawn_105, aes(x = atu_105, y = value))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", formula = y~x)+
  theme_minimal()+
  labs(x = "Accumulated Thermal Units", y = "Peak emergence (day of year)")

ggsave("../../output/regression_w_atu_105.jpeg",height = 10, width = 10)

#new file with atu for April 17, or doy=105
write.csv(peak_w_atu_spawn_105, "../../data/peaks_phenomix_w_covariates.csv")

