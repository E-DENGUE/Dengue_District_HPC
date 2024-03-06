##In console:
# salloc
# module load R/4.2.0-foss-2020b
# R

#setwd("~/project/dengue_test/Cluster_DW")


library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)

N_cores = detectCores()

file.names <- list.files('./Results')

ds.list <- mclapply(file.names,function(X){
  
  d1 <- readRDS(file=file.path(paste0('./Results/',X)))
  
  modN <-  sub("^(.*?)_.*$", "\\1", X)
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Extract the date from the string using gsub
  date <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$ds$preds %>%
    cbind.data.frame(d1$ds) %>%
    rename(pred=mean, pred_lcl=`0.025quant`, pred_ucl= `0.975quant`) %>%
    filter(forecast==1) %>%
    dplyr::select(pred, pred_lcl, pred_ucl ) 
  
  
  out.list <- cbind.data.frame( d1$scores, preds_df) %>%
  #  dplyr::select(date,  horizon, district, province, pred,m_DHF_cases,m_DHF_cases_hold, pop, crps1, crps2,pred,pred_lcl,pred_ucl) %>%
    mutate(form=d1$form, 
           eval.date=date,
           modN=modN)
  return(out.list)
},  mc.cores=N_cores)


out.slim<-  bind_rows(ds.list) %>%
  dplyr::select(date, modN, eval.date,horizon, district, province, pred,m_DHF_cases,m_DHF_cases_hold, pop, crps1, crps2,pred,pred_lcl,pred_ucl, form) %>%
 saveRDS(., "./cleaned_scores/all_crps_slim.rds")

scores <-  bind_rows(ds.list) %>%
    group_by(modN) %>%
    summarize(score_sum=mean(crps1) , n.obs=n())
    
scores_month <-  bind_rows(ds.list) %>%
    group_by(modN, monthN, horizon) %>%
    summarize(score_sum=mean(crps1) , n.obs=n()) %>%
    arrange(horizon, monthN, score_sum)
print(scores_month, n=1000)

scores_filtered <-  bind_rows(ds.list) %>%
group_by(horizon,modN) %>%
     summarize(score_sum=mean(crps1) , n.obs=n()) %>%
     ungroup() %>%
    arrange(horizon,score_sum)

print(scores_filtered, n=100)

N_date <-  bind_rows(ds.list) %>%
group_by(date) %>%
 summarize(N_mods_date= n()) %>%
 ungroup()

scores_filtered2  <-  bind_rows(ds.list) %>%
    left_join(N_date, by='date') %>%
    filter(N_mods_date == max(N_mods_date)) %>%
group_by(district,horizon,modN) %>%
     summarize(score_sum=mean(crps1) , n.obs=n()) %>%
        # filter(  n.obs==max( n.obs)) %>%
     ungroup() %>%
    arrange(district,horizon,score_sum) %>%
    group_by(district,horizon) %>%
    mutate(model_rank=row_number()) %>%
    ungroup() 
    

#WHAT ARE THE TOP MODELS BASED ON CRPS AT 1 and 2 MONTH AHEAD?
scores_filtered2  <-  bind_rows(ds.list) %>%
    left_join(N_date, by='date') %>%
    filter(N_mods_date == max(N_mods_date)) %>%
group_by(horizon,modN) %>%
     summarize(score_sum=mean(crps1) , n.obs=n()) %>%
        # filter(  n.obs==max( n.obs)) %>%
     ungroup() %>%
    arrange(horizon,score_sum) %>%
    group_by(horizon) %>%
    mutate(model_rank=row_number()) %>%
    ungroup() %>%
    print(., n=1000)
    

##DESKTOP EVALUATION OF OUTPUTS

out <- readRDS( "./cleaned_scores/all_crps_slim.rds")

miss.dates <- out %>% group_by(date, horizon) %>%   
  filter(!(modN %in% c('mod31','mod32')) & !is.na(crps2)) %>%
  summarize(N_mods=n(),N_cases=sum(m_DHF_cases )) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()

#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#Overall
out2 <- out %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & !(modN %in% c('mod31','mod32','mod39')) & !is.na(crps2)) %>%
  group_by(horizon, modN, form) %>%
  summarize(crps1 = mean(crps1),crps2 = mean(crps2) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) %>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )
View(out2)

#what model factors are associate with a higher weight?
mod1 <- lm(w_i2 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=out2)
summary(mod1)

#By calendar month
out3 <- out %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & !(modN %in% c('mod31','mod32'))& !is.na(crps2)) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )
View(out3)

#how much does inclusion of different components affect model weight?
mods <- out3 %>%
  ungroup() %>% 
  nest_by(month) %>%
  mutate(mod = list(lm(w_i2 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=data))) %>%
  dplyr::reframe(broom::tidy(mod)) %>%
  mutate(p.value=round(p.value,3))
View(mods)


#Observed vs expected

p1 <- out %>%
  filter(modN=='mod33' & horizon==2) %>%
  dplyr::select(-form) %>%
  mutate(pred_count =exp(pred)*pop/100000) %>%
  group_by(date) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_count)) %>%
  ggplot(aes(x=date, y=m_DHF_cases)) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_line(aes(x=date, y=pred_count), col='red')
  
# miss_pattern <- out %>% 
#   group_by(date, modN, horizon) %>%
#   summarize(N=n()) %>%
#   reshape2::dcast(., date + horizon~modN) %>%
#   arrange(horizon, date)





# ds <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
# 
# View(ds %>% group_by(date) %>% summarize(N=n()))
