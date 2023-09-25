#remotes::install_github("ericward-noaa/phenomix",build_vignettes = TRUE)
#install.packages("TMB", type = "source")
library(phenomix)
library(lubridate)
library(tidyverse)

plot_diagnostics <- function(fitted, type = "timing", logspace = TRUE, means=means) {

  # rebuild data frame
  df <- predict(fitted)

  # join in mean
  mus <- data.frame(
    years = unique(df$years),
    mu = fitted$sdreport$value[which(names(fitted$sdreport$value) == "mu")]
  )
  df <- left_join(df, mus)
  df$timing <- as.factor(ifelse(df$x < df$mu, "pre", "post"))

  if (type == "scatter") {
    if (logspace == TRUE) {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(pred, log(y), fill = timing, col = timing)) +
          geom_point( alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(log(pred), log(y), fill = timing, col = timing)) +
          geom_point( alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
    } else {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(exp(pred), y, fill = timing, col = timing)) +
          geom_point( alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Ln predicted") +
          ylab("Ln obs")
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(pred, y, fill = timing, col = timing)) +
          geom_point( alpha = 0.5) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          geom_abline(intercept = 0, slope = 1) +
          xlab("Predicted") +
          ylab("Obs")
      }
    }
  }
  if (type == "timing") {
    if (logspace == TRUE) {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(x, pred, fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, log(y), fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")+
          geom_vline(data = means ,aes(xintercept = value))+
          geom_rect(data = means,
                    aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, x= NULL, y = NULL),
                    fill = "grey", col = "grey", alpha = 0.5)
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(x, log(pred), fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          xlab("Calendar day") +
          ylab("Ln pred and obs") +
          geom_point(aes(x, log(y), fill = timing, col = timing), size = 1, alpha = 0.5) +
          geom_line(col = "black")+
          geom_vline(data = means ,aes(xintercept = value))+
          geom_rect(data = means,
                    aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, x= NULL, y = NULL),
                    fill = "grey", col = "grey", alpha = 0.5)
      }
    } else {
      if(fitted$data_list$family %in% c(2,3,5)) {
        g <- ggplot(df, aes(x, exp(pred), fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          xlab("Calendar day") +
          ylab("Predicted and observed catch per hour") +
          geom_point(aes(x, y, fill = timing, col = timing), size = 2, alpha = 0.5) +
          geom_line(col = "black")+
          geom_vline(data = means ,aes(xintercept = value))+
          geom_rect(data = means,
                    aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, x= NULL, y = NULL),
                    fill = "grey", col = "grey", alpha = 0.5)
      }
      if(fitted$data_list$family %in% c(1)) {
        g <- ggplot(df, aes(x, pred, fill = timing, col = timing)) +
          facet_wrap(~years, labeller = labeller(years =
                                                   c("1" = "2004",
                                                     "2" = "2005",
                                                     "3" = "2006",
                                                     "4" = "2007",
                                                     "5" = "2008",
                                                     "6" = "2009",
                                                     "7" = "2010",
                                                     "8" = "2011",
                                                     "9" = "2012",
                                                     "10" = "2013",
                                                     "11" = "2014",
                                                     "12" = "2015",
                                                     "13" = "2016",
                                                     "14" = "2017",
                                                     "15" = "2018",
                                                     "16" = "2019",
                                                     "17" = "2020",
                                                     "18" = "2021",
                                                     "19" = "2022")), scales = "free") +
          xlab("Calendar day") +
          ylab("Predicted and cbserved catch per hour") +
          geom_point(aes(x, y, fill = timing, col = timing), size = 2, alpha = 0.5) +
          geom_line(col = "black")+
          geom_vline(data = means ,aes(xintercept = value))+
          geom_rect(data = means,
                    aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, x= NULL, y = NULL),
                    fill = "grey", col = "grey", alpha = 0.5)
      }
    }
  }
  return(g)
}


fry_data_sum <- read.csv("../../data/fry_data_sum.csv")

cov_dat = data.frame(nyear = unique(fry_data_sum$year))
# rescale year -- could also standardize with scale()
cov_dat$nyear = cov_dat$nyear - min(cov_dat$nyear)

fry_data_sum$doy <- yday(fry_data_sum$date)

datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       asymmetric_model = FALSE,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
fitted <- fit(datalist)
#check for convergence
fitted$pars$convergence

g = plot_diagnostics(fitted, type="timing", logspace=TRUE)
g2 = plot_diagnostics(fitted, type="timing", logspace=F)
g
g2

extractAIC(fitted)
#523

datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       asymmetric_model = T,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gaussian")
fitted_asymmetric <- fit(datalist)

plot_diagnostics(fitted_asymmetric)

extractAIC(fitted_asymmetric)
#555

datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       asymmetric_model = F,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "gnorm")
fitted_gnorm_symmetric <- fit(datalist)

fitted_gnorm_symmetric$pars$convergence
#did not converge

#trying student t

datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       asymmetric_model = F,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
fitted_student_t_symmetric <- fit(datalist)

fitted_student_t_symmetric$pars$convergence

plot_diagnostics(fitted_student_t_symmetric, logspace = F)

extractAIC(fitted_student_t_symmetric)
#477

#let's try asymmetric
datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       asymmetric_model = T,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
fitted_student_t_asymmetric <- fit(datalist)


fitted_student_t_asymmetric$pars$convergence

plot_diagnostics(fitted_student_t_asymmetric, type = "timing", logspace = F, means)
ggsave("../../output/phenomix_peak_estimates_w_error.jpeg",height = 10, width = 10)
extractAIC(fitted_student_t_asymmetric)
#468
#that's the best so far

means <- extract_means(fitted_student_t_asymmetric)

means$year <- seq(2004,2022,1)
means$years <- seq(1,19,1)

means$peak <- as.Date(round(means$value,0),origin =  paste0(means$year,"-01-01"))
means$peak_lower <- as.Date(round(means$value - means$sd,0),origin =  paste0(means$year,"-01-01"))
means$peak_upper <- as.Date(round(means$value + means$sd,0),origin =  paste0(means$year,"-01-01"))

means$upper <- means$value + means$sd
means$lower <- means$value - means$sd

fry_data_means <- left_join(fry_data_sum,means,by = 'year')

ggplot(fry_data_means, aes(x  = doy, y = catch_per_hour,group = year))+
  geom_rect(data = means, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, x= NULL, y = NULL))+
  geom_point()+
  facet_wrap(vars(year))+
  geom_vline(data = means ,aes(xintercept = value))


write.csv(means,"../../data/peaks_phenomix.csv")

#trying different families

#neg binomial
datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       family = "negbin",
                       asymmetric_model = T,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
fitted_student_t_asymmetric_negbin <- fit(datalist)

fitted_student_t_asymmetric_negbin$pars$convergence
#did not converge


#gaussian
datalist = create_data(fry_data_sum,
                       min_number=1,
                       variable = "catch_per_hour",
                       time="year",
                       date = "doy",
                       family = "gaussian",
                       asymmetric_model = T,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
fitted_student_t_asymmetric_gaussian <- fit(datalist)

fitted_student_t_asymmetric_gaussian$pars$convergence
#did not converge

#poisson and using total_catch

datalist = create_data(fry_data_sum,
                       min_number=0,
                       variable = "total_catch",
                       time="year",
                       date = "doy",
                       family = "poisson",
                       asymmetric_model = T,
                       mu = ~ nyear,
                       sigma = ~ nyear,
                       covar_data = cov_dat,
                       est_sigma_re = TRUE,
                       est_mu_re = TRUE,
                       tail_model = "student_t")
fitted_student_t_asymmetric_poisson <- fit(datalist)

fitted_student_t_asymmetric_poisson$pars$convergence
