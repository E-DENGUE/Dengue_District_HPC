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
  dplyr::select(date, district,pop,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))


##Results from spatiotemporal models
file.names1 <- list.files('./Results/Results_spacetime/')
file.names2 <- paste0('./Results/Results_pca/',list.files('./Results/Results_pca'))

###########################
#First extract the CRPS summaries
############################
##summaries
ds.list1.summary <- lapply(file.names1,function(X){
  
  d1 <- readRDS(file=paste0('./Results/Results_spacetime/',file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
    # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  return(preds_df)
})


##Results from PCA aware analysis

ds.list2_summary <- lapply(file.names2,function(X){
  
  d1 <- readRDS(file=file.path(X))
  
  modN <-  'PC1'
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in) , #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           date.test.in=date.test.in,
           form=paste(d1$form, collapse=' '))

  return(preds_df)
})


summary1 <- lapply(ds.list1.summary, function(X){
  X$forecast=as.factor(X$forecast)
  return(X)
}) %>%
  bind_rows()

summary2 <- bind_rows(ds.list2_summary)

bind_rows(summary1,summary2) %>%
  filter(horizon>=1) %>%
  saveRDS( "./Results/all_crps_slim_updated.rds")

#########################################
## BRIER SCORES
#########################################
brier1 <- pblapply(file.names1,function(X){
  d1 <- readRDS(file=paste0('./Results/Results_spacetime/',file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(pred_epidemic_2sd = value > threshold,
           pred_epidemic_nb = value > threshold_nb,
           vintage_date=date.test.in) %>%
    group_by(date, vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
               prob_pred_epidemic_nb= mean(pred_epidemic_nb),
               obs_epidemic_2sd=mean(epidemic_flag),
               obs_epidemic_nb = mean(epidemic_flag_nb))
  
  brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
  brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
  
  brier.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd,'obs_epidemic_2sd'=pred.iter$obs_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd ,'obs_epidemic_nb'=pred.iter$obs_epidemic_nb )
})

brier2 <- pblapply(file.names2,function(X){
  d1 <- readRDS(file=file.path(X))
  
  modN <-  'PC1'
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    left_join(obs_epidemics, by=c('date','district')) %>%
    mutate(pred_epidemic_2sd = value > threshold,
           pred_epidemic_nb = value > threshold_nb,
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
               prob_pred_epidemic_nb= mean(pred_epidemic_nb),
               obs_epidemic_2sd=mean(epidemic_flag),
               obs_epidemic_nb = mean(epidemic_flag_nb))
  
  brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
  brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
  
  brier.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd,'obs_epidemic_2sd'=pred.iter$obs_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd ,'obs_epidemic_nb'=pred.iter$obs_epidemic_nb )
})



brier3 <- lapply(file.names3,function(X){
  
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
    mutate(pred_epidemic_2sd = value > log( threshold/pop*100000),
           pred_epidemic_nb = value > log( threshold_nb/pop*100000),
           vintage_date=date.test.in) %>%
    group_by(date,vintage_date, district, horizon) %>%
    summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
               prob_pred_epidemic_nb= mean(pred_epidemic_nb),
               obs_epidemic_2sd=mean(epidemic_flag),
               obs_epidemic_nb = mean(epidemic_flag_nb))
  
  brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
  brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
  
  brier.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd,'obs_epidemic_2sd'=pred.iter$obs_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd ,'obs_epidemic_nb'=pred.iter$obs_epidemic_nb )
})


#0=perfect prediction,1=bad
brier_summary <- c(brier1, brier2) %>% 
  bind_rows() %>%
  mutate(monthN=month(date))%>%
  ungroup() %>% 
  group_by(monthN,modN) %>%
  summarize(brier_nb=mean(brier_nb),
            brier_2sd =mean(brier_2sd))

saveRDS(brier_summary, "./Results/brier_summary_updated.rds")


##TEST PERFORMANCE IN A DIFFERENT WAY
#--evaluate the proportion of districts with a probability of outbreak > X actually had an observd outbreak
# mod1 <- mgcv::gam(obs_epidemic_2sd~s(prob_pred_epidemic_2sd), family='binomial', data=pred.iter)
#plot(pred.iter$prob_pred_epidemic_2sd, mod1$fitted.values, col=factor(pred.iter$horizon))
#abline(a=0, b=1)

# a1 <- pred.iter %>% 
#   filter(horizon==2 & district=='AN PHU')
# View(a1)  
# 
# 
# hist(a1$value)
# abline(v=mean(log(a1$threshold/a1$pop*100000)))
# 
# true_prop<- ls()
# values <- seq(0,1, by=0.01)
# for(j in 1:100) {
#   
#   i= values[j]
#   print(j)
#   
#   true_prop[[j]] <- d1$log.samps.inc %>%
#     reshape2::melt(., id.vars=c('date','district','horizon')) %>%
#     left_join(obs_epidemics, by=c('date','district')) %>%
#     mutate(pred_epidemic_2sd = value > log( threshold/pop*100000),
#            pred_epidemic_nb = value > log( threshold_nb/pop*100000),
#            vintage_date=date.test.in) %>%
#     group_by(date, vintage_date, district, horizon) %>%
#     summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
#                prob_pred_epidemic_nb= mean(pred_epidemic_nb),
#                obs_epidemic_2sd=mean(epidemic_flag),
#                obs_epidemic_nb = mean(epidemic_flag_nb)) %>%
#     ungroup() %>%
#     filter(prob_pred_epidemic_2sd> i ) %>% # look only at districts with probability above threshold
#     summarize(true_prop=mean(obs_epidemic_2sd)) %>%
#     pull(true_prop)
# }
# 
# plot(values[1:100], as.numeric(true_prop[1:100]))
# abline(a=0, b=1)
# hist(pred.iter$prob_pred_epidemic_2sd)
