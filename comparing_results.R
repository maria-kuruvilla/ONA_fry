#compare phenomix peaks with other model peaks

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

phenomix_peaks <- read.csv("../../data/peaks_phenomix.csv")

observed_peaks <- readRDS("../../data/observed_peak_emergence.rds")

predicted_peaks <- readRDS("../../data/predicted_emergence_w_bounds.rds")


#put them all in one dataframe

phenomix_dataframe <- bind_cols(year = phenomix_peaks$year,
                            phenomix_peak = as.Date(phenomix_peaks$peak),
                            phenomix_lower = as.Date(phenomix_peaks$peak_lower),
                            phenomix_upper = as.Date(phenomix_peaks$peak_upper))

#changing day of year se into dates
observed_peaks$observed_lower <- as.Date(round(observed_peaks$peak - observed_peaks$peak_se,0),
                                         origin =  paste0(observed_peaks$brood_year+1,"-01-01"))
observed_peaks$observed_upper <- as.Date(round(observed_peaks$peak + observed_peaks$peak_se,0),
                                         origin =  paste0(observed_peaks$brood_year+1,"-01-01"))


observed_dataframe <- bind_cols(observed_peak = observed_peaks$peak_date,
                            observed_lower = observed_peaks$observed_lower,
                            observed_upper = observed_peaks$observed_upper)

predicted_dataframe <- bind_cols(year =  predicted_peaks$brood_year+1,
                                 predicted_peak = as.Date(predicted_peaks$pred),
                                 predicted_lower = as.Date(predicted_peaks$lcb),
                                 predicted_upper = as.Date(predicted_peaks$ucb))


temp_dataframe <- bind_cols(phenomix_dataframe, observed_dataframe)

peaks <- left_join(temp_dataframe, predicted_dataframe)

ggplot(data = peaks)+
  geom_point(aes(x=year, y =yday(phenomix_peak)), col = "#1b9e77")+
  geom_point(aes(x=year, y =yday(observed_peak)), col = "#d95f02")+
  geom_point(aes(x=year, y =yday(predicted_peak)), col = "#7570b3")+
  labs(x = "Year", y = "Day of year")+
  theme_minimal()


peaks_longer <- peaks %>% pivot_longer(
  cols = phenomix_peak:predicted_upper,
  names_to = c("model",".value"),
  names_pattern = "(.*)_(.*)"
)

ggplot(data = peaks_longer,aes(x = year-1,
                               y = as.Date(yday(peak),origin = "2012-01-01"), col = model))+
  geom_point(size = 5,alpha = 0.7)+
  labs(x = "Brood Year", y = "Peak emergence (day of year)", col = "")+
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  theme(axis.text.x = element_text(size=50))+
  theme_minimal()+
  geom_errorbar(aes(ymin = as.Date(yday(lower),origin = "2012-01-01") ,
                    ymax = as.Date(yday(upper),origin = "2012-01-01")),
                    size=1, alpha = 0.7, width = 0.5)
ggsave("../../output/compare_peaks.jpeg",height = 6, width = 8)
