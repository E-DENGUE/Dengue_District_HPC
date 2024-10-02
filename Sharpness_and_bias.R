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

N_cores = detectCores()

obs_epidemics <- readRDS( './Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("district"="district","date"="date"))

##Results from spatiotemporal models
file.names1 <- list.files('./Results/Results_spacetime/')
file.names2 <- paste0('./Results/Results_pca/',list.files('./Results/Results_pca'))
file.names3 <- list.files('./Results/Results_hhh4')

###########################
#Find the Sharpness and bias summaries
############################
##summaries
calc_bias_sharpness1<- pblapply(file.names1,function(X){
  d1 <- readRDS(file=paste0('./Results/Results_spacetime/',file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(pred_epidemic = value < log(m_DHF_cases / pop * 100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic = mean(pred_epidemic)
    )
  
  pred.iter$bias <- 1 - 2 * pred.iter$prob_pred_epidemic
  
  
  sharpness_by_district <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate( vintage_date=date.test.in)%>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize(
      sharpness = {
        
        abs_deviations <- abs(value - median(value, na.rm = TRUE))
        
        mad <- median(abs_deviations, na.rm = TRUE)
        
        
        (1 / 0.675) * mad
      }
    )
  
  pred.iter$Sharpness<-  sharpness_by_district$sharpness
  
  
  bias.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, 'sharpness'=pred.iter$Sharpness, 'bias'=pred.iter$bias)
  
  return(bias.out) 
  
})



calc_bias_sharpness2 <- pblapply(file.names2,function(X){
  d1 <- readRDS(file=file.path(X))
  
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
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(pred_epidemic = value < log(m_DHF_cases / pop * 100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic = mean(pred_epidemic)
    )
  
  pred.iter$bias <- 1 - 2 * pred.iter$prob_pred_epidemic
  
  
  sharpness_by_district <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate( vintage_date=date.test.in)%>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize(
      sharpness = {
        
        abs_deviations <- abs(value - median(value, na.rm = TRUE))
        
        mad <- median(abs_deviations, na.rm = TRUE)
        
        
        (1 / 0.675) * mad
      }
    )
  
  pred.iter$Sharpness<-  sharpness_by_district$sharpness
  
  bias.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, 'sharpness'=pred.iter$Sharpness, 'bias'=pred.iter$bias)
  
  return(bias.out) 
  
})

calc_bias_sharpness3 <- lapply(file.names3,function(X){
  
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
    mutate(pred_epidemic = value < log(m_DHF_cases / pop * 100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic = mean(pred_epidemic)
    )
  
  pred.iter$bias <- 1 - 2 * pred.iter$prob_pred_epidemic
  
  
  sharpness_by_district <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate( vintage_date=date.test.in)%>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize(
      sharpness = {
        
        abs_deviations <- abs(value - median(value, na.rm = TRUE))
        
        mad <- median(abs_deviations, na.rm = TRUE)
        
        
        (1 / 0.675) * mad
      }
    )
  
  pred.iter$Sharpness<-  sharpness_by_district$sharpness
  
  bias.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, 'sharpness'=pred.iter$Sharpness, 'bias'=pred.iter$bias)
  
return(bias.out) 
  
})



#0=perfect prediction,1=bad
calc_bias_sharpness_summary <- c(calc_bias_sharpness1 , calc_bias_sharpness2,calc_bias_sharpness3 ) %>% 
  bind_rows() %>%
  mutate(monthN=month(date))%>%
  ungroup() 

saveRDS(calc_bias_sharpness_summary, "./Results/sharpness_bias_summary_all_dates.rds")



