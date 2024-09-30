##In console:
#salloc
#module load  R/4.2.3-foss-2022b
# R


library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
#library(gganimate)
library(pbapply)
library(scoringutils)
library(stringr)
options(dplyr.summarise.inform = FALSE)
library(reshape2)

N_cores = detectCores()

obs_epidemics <- readRDS( './Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  dplyr::rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("district"="district","date"="date"))

##Results from spatiotemporal models
file.names1 <- list.files('./Results/Results_spacetime/')
file.names2 <- paste0('./Results/Results_pca/',list.files('./Results/Results_pca'))
file.names3 <- list.files('./Results/Results_hhh4/')


prob1 <- pblapply(file.names1, function(X) {
  d1 <- readRDS(file = paste0('./Results/Results_spacetime/', file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(
      pred_outbreak_epidemic_2sd = quantile(value, probs = 0.50) > log(threshold / pop * 100000),
      pred_outbreak_epidemic_nb = quantile(value, probs = 0.50) > log(threshold_nb / pop * 100000),
      pred_outbreak_epidemic_poisson = quantile(value, probs = 0.50) > log(threshold_poisson / pop * 100000),
      pred_outbreak_epidemic_quant = quantile(value, probs = 0.50) > log(threshold_quant / pop * 100000),
      pred_outbreak_epidemic_fix_100 = quantile(value, probs = 0.50) > log(100),
      pred_outbreak_epidemic_fix_150 = quantile(value, probs = 0.50) > log(150),
      pred_outbreak_epidemic_fix_300 = quantile(value, probs = 0.50) > log(300),
      pred_outbreak_epidemic_fix_50 = quantile(value, probs = 0.50) > log(50),
      pred_outbreak_epidemic_fix_20 = quantile(value, probs = 0.50) > log(20),
      pred_outbreak_epidemic_fix_200 = quantile(value, probs = 0.50) > log(200),
      vintage_date = date.test.in
    ) %>%
    dplyr::group_by(date, vintage_date, district, horizon) %>%
    dplyr::summarize(
      outbreak_predicted_epidemic_2sd = mean(pred_outbreak_epidemic_2sd),
      outbreak_predicted_epidemic_nb = mean(pred_outbreak_epidemic_nb),
      outbreak_predicted_epidemic_poisson = mean(pred_outbreak_epidemic_poisson),
      outbreak_predicted_epidemic_quant = mean( pred_outbreak_epidemic_quant),
      outbreak_predicted_epidemic_fix_100 = mean(pred_outbreak_epidemic_fix_100),
      outbreak_predicted_epidemic_fix_150 = mean( pred_outbreak_epidemic_fix_150),
      outbreak_predicted_epidemic_fix_300 = mean(  pred_outbreak_epidemic_fix_300),
      outbreak_predicted_epidemic_fix_50 = mean(pred_outbreak_epidemic_fix_50),
      outbreak_predicted_epidemic_fix_20 = mean(pred_outbreak_epidemic_fix_20),
      outbreak_predicted_epidemic_fix_200 = mean(pred_outbreak_epidemic_fix_200),
      obs_epidemic_2sd=mean(epidemic_flag),
      obs_epidemic_nb = mean(epidemic_flag_nb),
      obs_epidemic_quant=mean(epidemic_flag_quant) ,
      obs_epidemic_poisson=mean(epidemic_flag_poisson)
    )
  
  prob.out <- cbind.data.frame(
    date = pred.iter$date, 
    modN = modN, 
    district = pred.iter$district, 
    horizon = pred.iter$horizon,
    outbreak_predicted_epidemic_2sd = pred.iter$outbreak_predicted_epidemic_2sd,
    outbreak_predicted_epidemic_nb = pred.iter$outbreak_predicted_epidemic_nb,
    outbreak_predicted_epidemic_poisson = pred.iter$outbreak_predicted_epidemic_poisson,
    outbreak_predicted_epidemic_quant = pred.iter$outbreak_predicted_epidemic_quant, 
    outbreak_predicted_epidemic_fix_100 = pred.iter$outbreak_predicted_epidemic_fix_100,
    outbreak_predicted_epidemic_fix_150 = pred.iter$outbreak_predicted_epidemic_fix_150, 
    outbreak_predicted_epidemic_fix_300 = pred.iter$outbreak_predicted_epidemic_fix_300,
    outbreak_predicted_epidemic_fix_50 = pred.iter$outbreak_predicted_epidemic_fix_50,
    outbreak_predicted_epidemic_fix_20 = pred.iter$outbreak_predicted_epidemic_fix_20,
    outbreak_predicted_epidemic_fix_200 = pred.iter$outbreak_predicted_epidemic_fix_200,
    obs_epidemic_2sd=pred.iter$obs_epidemic_2sd,
    obs_epidemic_nb = pred.iter$obs_epidemic_nb,
    obs_epidemic_quant=pred.iter$obs_epidemic_quant ,
    obs_epidemic_poisson=pred.iter$obs_epidemic_poisson
  )
  return(prob.out)
})

saveRDS(prob1, "./Results/predicted_outbreak1_50_percentile.rds")

prob2 <- pblapply(file.names2, function(X) {
  d1 <- readRDS(file = file.path(X))
  
  if (grepl("PC_lags_weather", X)) {
    modN <- "PC_lags_weather"
  } else if (grepl("PC_lags", X)) {
    modN <- "PC_lags"
  } else if (grepl("PC_weather", X)) {
    modN <- "PC_weather"
  } else {
    modN <- NA  # Handle other cases if necessary
  }
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using regmatches
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(
      pred_outbreak_epidemic_2sd = quantile(value, probs = 0.50) > log(threshold / pop * 100000),
      pred_outbreak_epidemic_nb = quantile(value, probs = 0.50) > log(threshold_nb / pop * 100000),
      pred_outbreak_epidemic_poisson = quantile(value, probs = 0.50) > log(threshold_poisson / pop * 100000),
      pred_outbreak_epidemic_quant = quantile(value, probs = 0.50) > log(threshold_quant / pop * 100000),
      pred_outbreak_epidemic_fix_100 = quantile(value, probs = 0.50) > log(100),
      pred_outbreak_epidemic_fix_150 = quantile(value, probs = 0.50) > log(150),
      pred_outbreak_epidemic_fix_300 = quantile(value, probs = 0.50) > log(300),
      pred_outbreak_epidemic_fix_50 = quantile(value, probs = 0.50) > log(50),
      pred_outbreak_epidemic_fix_20 = quantile(value, probs = 0.50) > log(20),
      pred_outbreak_epidemic_fix_200 = quantile(value, probs = 0.50) > log(200),
      vintage_date = date.test.in
    ) %>%
    dplyr::group_by(date, vintage_date, district, horizon) %>%
    dplyr::summarize(
      outbreak_predicted_epidemic_2sd = mean(pred_outbreak_epidemic_2sd),
      outbreak_predicted_epidemic_nb = mean(pred_outbreak_epidemic_nb),
      outbreak_predicted_epidemic_poisson = mean(pred_outbreak_epidemic_poisson),
      outbreak_predicted_epidemic_quant = mean( pred_outbreak_epidemic_quant),
      outbreak_predicted_epidemic_fix_100 = mean(pred_outbreak_epidemic_fix_100),
      outbreak_predicted_epidemic_fix_150 = mean( pred_outbreak_epidemic_fix_150),
      outbreak_predicted_epidemic_fix_300 = mean(  pred_outbreak_epidemic_fix_300),
      outbreak_predicted_epidemic_fix_50 = mean(pred_outbreak_epidemic_fix_50),
      outbreak_predicted_epidemic_fix_20 = mean(pred_outbreak_epidemic_fix_20),
      outbreak_predicted_epidemic_fix_200 = mean(pred_outbreak_epidemic_fix_200),
      obs_epidemic_2sd=mean(epidemic_flag),
      obs_epidemic_nb = mean(epidemic_flag_nb),
      obs_epidemic_quant=mean(epidemic_flag_quant) ,
      obs_epidemic_poisson=mean(epidemic_flag_poisson)
    )
  
  prob.out <- cbind.data.frame(
    date = pred.iter$date, 
    modN = modN, 
    district = pred.iter$district, 
    horizon = pred.iter$horizon,
    outbreak_predicted_epidemic_2sd = pred.iter$outbreak_predicted_epidemic_2sd,
    outbreak_predicted_epidemic_nb = pred.iter$outbreak_predicted_epidemic_nb,
    outbreak_predicted_epidemic_poisson = pred.iter$outbreak_predicted_epidemic_poisson,
    outbreak_predicted_epidemic_quant = pred.iter$outbreak_predicted_epidemic_quant, 
    outbreak_predicted_epidemic_fix_100 = pred.iter$outbreak_predicted_epidemic_fix_100,
    outbreak_predicted_epidemic_fix_150 = pred.iter$outbreak_predicted_epidemic_fix_150, 
    outbreak_predicted_epidemic_fix_300 = pred.iter$outbreak_predicted_epidemic_fix_300,
    outbreak_predicted_epidemic_fix_50 = pred.iter$outbreak_predicted_epidemic_fix_50,
    outbreak_predicted_epidemic_fix_20 = pred.iter$outbreak_predicted_epidemic_fix_20,
    outbreak_predicted_epidemic_fix_200 = pred.iter$outbreak_predicted_epidemic_fix_200,
    obs_epidemic_2sd=pred.iter$obs_epidemic_2sd,
    obs_epidemic_nb = pred.iter$obs_epidemic_nb,
    obs_epidemic_quant=pred.iter$obs_epidemic_quant ,
    obs_epidemic_poisson=pred.iter$obs_epidemic_poisson
  )
  return(prob.out)
})

saveRDS(prob2, "./Results/predicted_outbreak2_50_percentile.rds")


prob3 <- lapply(file.names3,function(X){
  
  d1 <- readRDS(file=file.path(paste0('./Results/Results_hhh4/',X)))
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(
      pred_outbreak_epidemic_2sd = quantile(value, probs = 0.50) > log(threshold / pop * 100000),
      pred_outbreak_epidemic_nb = quantile(value, probs = 0.50) > log(threshold_nb / pop * 100000),
      pred_outbreak_epidemic_poisson = quantile(value, probs = 0.50) > log(threshold_poisson / pop * 100000),
      pred_outbreak_epidemic_quant = quantile(value, probs = 0.50) > log(threshold_quant / pop * 100000),
      pred_outbreak_epidemic_fix_100 = quantile(value, probs = 0.50) > log(100),
      pred_outbreak_epidemic_fix_150 = quantile(value, probs = 0.50) > log(150),
      pred_outbreak_epidemic_fix_300 = quantile(value, probs = 0.50) > log(300),
      pred_outbreak_epidemic_fix_50 = quantile(value, probs = 0.50) > log(50),
      pred_outbreak_epidemic_fix_20 = quantile(value, probs = 0.50) > log(20),
      pred_outbreak_epidemic_fix_200 = quantile(value, probs = 0.50) > log(200),
      vintage_date = date.test.in
    ) %>%
    dplyr::group_by(date, vintage_date, district, horizon) %>%
    dplyr::summarize(
      outbreak_predicted_epidemic_2sd = mean(pred_outbreak_epidemic_2sd),
      outbreak_predicted_epidemic_nb = mean(pred_outbreak_epidemic_nb),
      outbreak_predicted_epidemic_poisson = mean(pred_outbreak_epidemic_poisson),
      outbreak_predicted_epidemic_quant = mean( pred_outbreak_epidemic_quant),
      outbreak_predicted_epidemic_fix_100 = mean(pred_outbreak_epidemic_fix_100),
      outbreak_predicted_epidemic_fix_150 = mean( pred_outbreak_epidemic_fix_150),
      outbreak_predicted_epidemic_fix_300 = mean(  pred_outbreak_epidemic_fix_300),
      outbreak_predicted_epidemic_fix_50 = mean(pred_outbreak_epidemic_fix_50),
      outbreak_predicted_epidemic_fix_20 = mean(pred_outbreak_epidemic_fix_20),
      outbreak_predicted_epidemic_fix_200 = mean(pred_outbreak_epidemic_fix_200),
      obs_epidemic_2sd=mean(epidemic_flag),
      obs_epidemic_nb = mean(epidemic_flag_nb),
      obs_epidemic_quant=mean(epidemic_flag_quant) ,
      obs_epidemic_poisson=mean(epidemic_flag_poisson)
    )
  
  prob.out <- cbind.data.frame(
    date = pred.iter$date, 
    modN = modN, 
    district = pred.iter$district, 
    horizon = pred.iter$horizon,
    outbreak_predicted_epidemic_2sd = pred.iter$outbreak_predicted_epidemic_2sd,
    outbreak_predicted_epidemic_nb = pred.iter$outbreak_predicted_epidemic_nb,
    outbreak_predicted_epidemic_poisson = pred.iter$outbreak_predicted_epidemic_poisson,
    outbreak_predicted_epidemic_quant = pred.iter$outbreak_predicted_epidemic_quant, 
    outbreak_predicted_epidemic_fix_100 = pred.iter$outbreak_predicted_epidemic_fix_100,
    outbreak_predicted_epidemic_fix_150 = pred.iter$outbreak_predicted_epidemic_fix_150, 
    outbreak_predicted_epidemic_fix_300 = pred.iter$outbreak_predicted_epidemic_fix_300,
    outbreak_predicted_epidemic_fix_50 = pred.iter$outbreak_predicted_epidemic_fix_50,
    outbreak_predicted_epidemic_fix_20 = pred.iter$outbreak_predicted_epidemic_fix_20,
    outbreak_predicted_epidemic_fix_200 = pred.iter$outbreak_predicted_epidemic_fix_200,
    obs_epidemic_2sd=pred.iter$obs_epidemic_2sd,
    obs_epidemic_nb = pred.iter$obs_epidemic_nb,
    obs_epidemic_quant=pred.iter$obs_epidemic_quant ,
    obs_epidemic_poisson=pred.iter$obs_epidemic_poisson
  )
  return(prob.out)
})

saveRDS(prob3, "./Results/predicted_outbreak3_50_percentile.rds")

#0=perfect prediction,1=bad
prob_summary <- c(prob1, prob2,prob3) %>% 
  bind_rows() %>%
  mutate(monthN=month(date))%>%
  ungroup() 

saveRDS(prob_summary, "./Results/predicted_outbreak_50_percentile.rds")



