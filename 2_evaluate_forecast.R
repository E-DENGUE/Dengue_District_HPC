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
library(plotly)
library(viridis)
library(lubridate)
library(gifski)
library(gganimate)

N_cores = detectCores()

##Results from spatiotemporal models
file.names1 <- list.files('./Results')

ds.list1 <- lapply(file.names1,function(X){
  
  d1 <- readRDS(file=paste0('./Results/',file.path(X)))
  
  modN <-  sub("^(.*?)_.*$", "\\1", X)
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in) %m-% months(1), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           date.test.in=date.test.in,
           form=d1$form)
    
   
  return(preds_df)
})

##Results from PCA aware analysis
  file.names2 <- paste0('./Results_b/',list.files('./Results_b'))
  
ds.list2 <- lapply(file.names2,function(X){
  
  d1 <- readRDS(file=file.path(X))
  
  modN <-  'PC1'
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  preds_df <- d1$scores %>%
    mutate(vintage_date=as.Date(date.test.in) %m-% months(1), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           date.test.in=date.test.in,
           form=paste(d1$form, collapse=' '))
  
  
  return(preds_df)
})

 bind_rows(c(ds.list1,ds.list2)) %>%
  filter(horizon>=1) %>%
 saveRDS( "./cleaned_scores/all_crps_slim.rds")


##################################
##DESKTOP EVALUATION OF OUTPUTS
############################################
 ##NOTE THIS METHOD OF WEIGHTING THE ENSEMBLE IS NOT OPIMAL--JUST A PLACEHOLDER--NEED TO USE FULL POSTERIOR WITH AN OPTIMIZATION ROUTINE##
 ##THIS METHOD WAS EMPLOYED IN https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003542
 #epiforecasts::stackr or a method that uses the quantile summary of the posterior would be preferred
 
 
obs_case <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

out <- readRDS( "./cleaned_scores/all_crps_slim.rds") %>%  #CRPS score from model
  dplyr::select(-pop,-m_DHF_cases) %>%
  full_join(obs_case, by=c('date','district'))

obs_epidemics <- readRDS( './Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

miss.dates <- out %>% 
  group_by(date, horizon) %>%   
  filter(!(modN %in% c('mod31','mod32', 'mod39')) & horizon %in% c(1,2)) %>%
  summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()

miss.mod <- out %>%
  filter(horizon==2) %>%
  group_by(modN) %>%
  summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N))


#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%
 # filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN OBSERVED IN THE DISTRICT

View(out_1a %>% group_by(district,date, horizon) %>% summarize(N=n()))


#Overall
out2 <- out_1a %>%
  filter(epidemic_flag==0) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & !(modN %in% c('mod39')) ) %>%
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
mod1 <- lm(w_i1 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=out2)
summary(mod1)

#By calendar month
out3 <- out_1a %>%
  filter(epidemic_flag==0) %>%
  filter(!(modN %in% c('mod31','mod32', 'mod39')) & !is.na(crps2)) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 ) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),
         w_i2 = (1/crps2^2)/sum(1/crps2^2) )%>%
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


## How does best model differ by district?
out4 <- out_1a %>%
  filter(epidemic_flag==0) %>%
  filter(!(modN %in% c('mod31','mod32', 'mod39')) & !is.na(crps2)) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 ) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, district, modN, form) %>%
  summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,district, crps2)%>%
  ungroup() %>%
  group_by(horizon,district) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2), rel_wgt2= w_i2/max(w_i2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  ) %>%
  dplyr::select(-form) %>%
  ungroup() %>%
  arrange(district, crps2) %>%
  group_by(district) %>%
  mutate(mod_rank = row_number()) %>%
  ungroup()

out4_ranks <- out4 %>% group_by(modN) %>% summarize(ave_rank=mean(mod_rank), min_rank=min(mod_rank), max=max(mod_rank))


View(out4)

out4%>%
ggplot(aes(x=modN,y=rel_wgt2, group=district )) +
  geom_line()+
  theme_classic()

ggplot(out4, aes(x = district, y = modN, fill = rel_wgt2)) +
  geom_tile() +
  scale_fill_viridis(discrete = FALSE)+
  labs(title = "CRPS Heatmap",
       x = "District",
       y = "ModelN")

#mean of relative weghts across all districts--this basically agrees with what is seen in out2
out4 %>% group_by(modN) %>% summarize(rel_wgt2=mean(rel_wgt2)) %>% arrange(-rel_wgt2)

#how much does inclusion of different components affect model weight?
mods <- out3 %>%
  ungroup() %>% 
  nest_by(month) %>%
  mutate(mod = list(lm(w_i2 ~ rw_season + harm_season+lag2_y + lag2_monthly_cum_ppt + rw_time_spatial + type4_spatial_bym, data=data))) %>%
  dplyr::reframe(broom::tidy(mod)) %>%
  mutate(p.value=round(p.value,3))
View(mods)

#################################################
#Observed vs expected

mod.weights_overall <- out2 %>%
  ungroup() %>%
  dplyr::select( w_i2 ,modN)

#ensemble, weight based on overall performance
p0.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 & !(modN %in% c( 'mod39', 'mod31','mod32'))) %>%
  dplyr::select(-form) %>%
  group_by(modN,date) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_overall, by=c('modN')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date) %>%
  mutate(ensemble_overall = sum(w_i2/sum(w_i2) *pred_count) ) %>%
  dplyr::select(date,modN ,ensemble_overall)

#ensemble; varying weights by calendar month
mod.weights_t <- out3 %>%
  ungroup() %>%
  filter(horizon==2) %>%
  dplyr::select(w_i2, modN,  month)

p1.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 & !(modN %in% c( 'mod39', 'mod31','mod32'))) %>%
  dplyr::select(-form) %>%
  group_by(modN,date) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date) %>%
  mutate(ensemble_month = sum(w_i2/sum(w_i2) *pred_count) )

p1 <- p1.ds %>%
  #filter(modN=='mod33') %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_month,group=modN), alpha=0.5 ,lwd=1, col='red')

p1
ggplotly(p1)  

#########################
#same but weights vary by district
mod.weights_dist<- out4 %>%
  ungroup() %>%
  filter(horizon==2) %>%
  dplyr::select(w_i1, modN,   district) %>%
  arrange(district, -w_i1)

p2.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 & !(modN %in% c( 'mod39', 'mod31','mod32'))) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,district) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, district) %>%
  summarize(ensemble_dist_wgt = sum(w_i1/sum(w_i1) *pred_count) #summarize across the different models to get date and district-specific estimate
         ) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize( ensemble_dist_wgt=sum(ensemble_dist_wgt)) %>% #sum across the districts
  right_join(p1.ds, by='date') %>%
  left_join(p0.ds, by=c('date', 'modN'))


p2.ensembles <- p2.ds %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA) +
 # geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_dist_wgt,group=modN,), alpha=0.5 ,lwd=1, col='red')+ #weight by 
geom_line(aes(x=date, y=ensemble_month,group=modN,), alpha=0.5 ,lwd=1, col='blue')+
  geom_line(aes(x=date, y=ensemble_overall,group=modN,), alpha=0.5 ,lwd=1, col='orange')+
  ggtitle("Differential weighting of ensemble has little effect")

p2.ensembles #the 3 ensembles looks almost identical


#ggplotly(p2)  
## DISTRICT-LEVEL PLOTS--make gifs showing observed and expected

  gif.ds <- out_1a %>%
    left_join(obs_case, by=c('date','district')) %>%
    filter(horizon==2 & modN %in% c('mod25','mod33','PC1'))
  
    all.districts <- unique(out$district)
    
    plot.dist.fun <- function(district.select){
  
       p2 <- gif.ds %>%
        filter(district %in% district.select) %>%
         ungroup() %>%
        ggplot(aes(x=date, y=m_DHF_cases, group=modN), size=2, color='black') +
         geom_line() +
        theme_classic()+
        ylim(0,NA)+
        geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN, group=modN),linetype=0,alpha=0.1)+
         geom_line(aes(x=date, y=pred_mean, group=modN, color=modN, linetype=modN), alpha=0.5,color='red')+
         ggtitle('Ensemble')+
        geom_hline(yintercept=43.75, col='gray', lty=2)+
         ggtitle(district.select) +
         transition_reveal(as.Date(date))
       animate(p2, fps=5, nframes=100, renderer = gifski_renderer())
       
       anim_save(paste0("./obs_exp_gifs/obs_exp_",district.select, ".gif"))
       
    }
    
    all.plots <- lapply(all.districts,plot.dist.fun)
    

    
    
obs_vs_expected_district <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 &  modN=='mod19' ) %>%
  group_by(district) %>%
  summarize(obs=sum(m_DHF_cases), pred=sum(pred_mean)) %>%
  mutate(diff= obs - pred, rr=obs/pred)
  
p2 <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 &  modN=='mod19'  & district %in% c('CHO MOI')) %>%
  dplyr::select(-form) %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_point() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~district,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl),alpha=0.2)+
  ggtitle('Model 19')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2



# ds <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
# 
# View(ds %>% group_by(date) %>% summarize(N=n()))
