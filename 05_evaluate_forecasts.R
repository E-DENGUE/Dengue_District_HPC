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
library(gganimate)
library(pbapply)
library(scoringutils)
#library(patchwork)
options(dplyr.summarise.inform = FALSE)
library(dplyr)
library(GGally)

library(ggpubr)
library(sf)
###Combined the all results form the three ways.
# summaryhhh4<- readRDS("all_crps_slim_hhh4.rds")
# summary_inla_pca<- readRDS("all_crps_slim_INLA_PCA.rds")
# 
# bind_rows(summaryhhh4,summary_inla_pca) %>%
#   filter(horizon>=1) %>%
#   saveRDS( "all_crps_slim_updated.rds")

N_cores = detectCores()

obs_epidemics <- readRDS( './Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

out <- readRDS( "./Data/cleaned_scores/all_crps_slim_updated_addpca2.rds") %>%  #CRPS score from model
  dplyr::select(-pop,-m_DHF_cases) %>%
  full_join(obs_case, by=c('date','district')) %>%
  filter(date<='2016-12-01')

#View(out %>% filter(is.na(m_DHF_cases)))

m<- out[out$modN=='mod48_',]
date_sequence <- seq.Date(from = as.Date("2012-01-01"), to = as.Date("2016-12-01"), by = "month")

q<- setdiff(date_sequence,as.Date(m$vintage_date))
as.Date(q)

miss.mod <- out %>%
  filter(horizon==2 & date <= '2016-10-01' & date !='2014-03-01' & date != '2014-04-01') %>%
  group_by(modN) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N))

miss.dates <- out %>% 
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
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

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%
#filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN OBSERVED IN THE DISTRICT

View(out_1a %>% group_by(district,date, horizon) %>% summarize(N=n()))


#Overall
out2 <- out_1a %>%
  filter(epidemic_flag==0) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  group_by(horizon, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2 = mean(crps2) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  dplyr::mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) %>%
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
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter(epidemic_flag==0) %>%
  filter( !is.na(crps2)) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  dplyr:: summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
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
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0 &!is.na(crps2))  %>%
  filter(epidemic_flag==0) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, district, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
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

## cluster of models, base don performance across districts
out4.c <- reshape2::dcast(out4, modN~district, value.var= 'rel_wgt2' ) 

out4.c.m <- out4.c %>% dplyr::select(-modN) %>% as.matrix()

row.names(out4.c.m) <- out4.c$modN
dist_mat <- dist(as.matrix(out4.c.m), method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 4)

cluster_mods <- cbind.data.frame(modN=names(cut_avg), clustN=cut_avg) %>%
  left_join(out2, by='modN') %>%
  arrange(clustN, crps2) %>%
  group_by(clustN) %>%
  mutate(group_order=row_number()) %>%
  ungroup()

##MODELS TO SELECT
#Select mod1 in cluster 1; 
#mod18 in cluster 2
#mod49 in cluster 3
#PC1 in cluster 3
#modhhh4_power_precip_temp_ from cluster 4

ensemble_mods <- c ('mod6_','mod60_','mod61_','PC_lags','modhhh4_power_precip_temp_')

## reverse now: clusterof districts, based on how different models perform
out4.c.map <- reshape2::dcast(out4, district~modN, value.var= 'rel_wgt2' ) 

out4.c.map.m <- out4.c.map %>% dplyr::select(-district) %>% as.matrix()

row.names(out4.c.map.m) <- out4.c.map$district
dist_mat_map <- dist(as.matrix(out4.c.map.m), method = 'euclidean')

hclust_avg_map <- hclust(dist_mat_map, method = 'average')
plot(hclust_avg_map)
cut_avg_map <- cutree(hclust_avg_map, k = 4)
cluster.map <- cbind.data.frame(district=names(cut_avg_map), clustN=cut_avg_map)

#MAP THESE CLUSTER ASSIGNMENTS


out4 %>%
  reshape2::dcast(district~modN, value.var='rel_wgt2') %>%
  dplyr::select(-district) %>%
  as.matrix() %>%
  cor() 

#mean of relative weights across all districts--this basically agrees with what is seen in out2
out4 %>% group_by(modN) %>% dplyr::summarize(rel_wgt2=mean(rel_wgt2)) %>% arrange(-rel_wgt2)

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
  dplyr::select( w_i2 ,modN) %>%
  filter(modN %in% ensemble_mods) %>%
  mutate(w_i2 = w_i2/sum(w_i2))

#ensemble, weight based on overall performance
p0.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 ) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_overall, by=c('modN')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, vintage_date) %>%
  mutate(sum_wgts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_overall = sum(w_i2/sum_wgts *pred_count) ) %>%
  dplyr::select(date,vintage_date,ensemble_overall)

#ensemble; varying weights by calendar month
mod.weights_t <- out3 %>%
  ungroup() %>%
  filter(modN %in% ensemble_mods) %>%
  filter(horizon==2) %>%
  dplyr::select(w_i2, modN,  month)

p1.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 ) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, vintage_date) %>%
  mutate(sum_wgts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count),
                   m_DHF_cases=mean(m_DHF_cases),pop=mean(pop)) %>%
  ungroup() %>%
  arrange( date)

p1a <- p1.ds %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  #geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_month), alpha=0.5 ,lwd=1, col='red')+
  ggtitle("Prediction Accuracy")



p1b <- p1.ds %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  #geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=vintage_date, y=ensemble_month), alpha=0.5 ,lwd=1, col='red')+
  ggtitle("Predictions by Vintage Date")

p1a
p1b
ggplotly(p1b)  


#########################
#same but weights vary by district
mod.weights_dist<- out4 %>%
  ungroup() %>%
  filter(horizon==2) %>%
  dplyr::select(w_i2, modN,   district) %>%
  arrange(district, -w_i2)

p2.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 & modN %in% ensemble_mods ) %>% #RESTRICTS TO THE SELECTED ENSEMBLE
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,district) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, district) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count) #summarize across the different models to get date and district-specific estimate
  ) %>%
  ungroup() %>%
  group_by(date,vintage_date) %>%
  summarize( ensemble_dist_wgt=sum(ensemble_dist_wgt)) %>% #sum across the districts
  right_join(p1.ds, by='date') %>%
  left_join(p0.ds, by=c('date'))


p2.ensembles <- p2.ds %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA) +
  # geom_line(aes(x=date, y=pred_count,group=modN, color=modN), lwd=0.5, alpha=0.5) +
  geom_line(aes(x=date, y=ensemble_dist_wgt,), alpha=0.5 ,lwd=1, col='red')+ #weight by district
  geom_line(aes(x=date, y=ensemble_month,), alpha=0.5 ,lwd=1, col='blue')+ #weight by calendar month 
  geom_line(aes(x=date, y=ensemble_overall), alpha=0.5 ,lwd=1, col='orange')+
  ggtitle("Differential weighting of ensemble has little effect")

p2.ensembles #the first 2 ensembles looks almost identical; weighting by district slightly different



obs_vs_expected_district <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 &  modN %in% ensemble_mods ) %>%
  group_by(district, modN) %>%
  summarize(obs=sum(m_DHF_cases), pred=sum(pred_mean)) %>%
  mutate(diff= obs - pred, rr=obs/pred)

p2.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  dplyr::select(-form) 

#choi moi in cluster 1
p2a <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & district %in% c('CHO MOI')) %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~district,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('various models')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2a

#Ben luc in cluster 2
p2b <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & district %in% c('BEN LUC')) %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~district,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('various models')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2b

#BAC LIEU in cluster 3
p2c <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & district %in% c('BAC LIEU')) %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~district,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('various models')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2c

#CAI LAY in cluster 3
p2d <- p2.ds %>%
  filter( horizon==2 &  modN %in% ensemble_mods  & district %in% c('CAI LAY')) %>%
  ggplot(aes(x=date, y=m_DHF_cases), lwd=4) +
  geom_line() +
  theme_classic()+
  ylim(0,NA)+
  geom_point(aes(x=date, y=pred_mean,group=modN, color=modN, alpha=0.5))+
  facet_wrap(~district,nrow=2) +
  geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl, fill=modN),alpha=0.2)+
  ggtitle('various models')+
  geom_hline(yintercept=43.75, col='gray', lty=2)
p2d


#### Ensemble performance by district
district_select <- c('CHO MOI','BEN LUC','BAC LIEU','CAI LAY')

p2.ds_district <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 ) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,district) %>%
  summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  filter(  modN %in% ensemble_mods ) %>%
  ungroup() %>%
  group_by(date,vintage_date, district) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count), m_DHF_cases=mean(m_DHF_cases) #summarize across the different models to get date and district-specific estimate
  ) %>%
  filter(district %in% district_select )

p2.ds_district %>%
  ggplot(aes(x=date, y=m_DHF_cases)) +
  geom_line()+
  geom_line(aes(x=date, y=ensemble_dist_wgt), color='red')+
  facet_wrap(~district)+
  theme_minimal()

#b vintage_date--would want red line to be ahead of black line
p2.ds_district %>%
  ggplot(aes(x=date, y=m_DHF_cases)) +
  geom_line()+
  geom_line(aes(x=vintage_date, y=ensemble_dist_wgt), color='red')+
  facet_wrap(~district)+
  theme_minimal() +
  ggtitle('Vintage date of forecats vs obs date')


################################################
##BRIER SCORE
################################################
#Shows poor performance during time of year with little dengue; good performance
#when  in dengue season
b1 <- readRDS('./Data/cleaned_scores/brier_summary_updated_Final.rds')

p1 <- b1 %>%
  group_by(monthN,modN) %>%
  summarize(brier_2sd=mean(brier_2sd)) %>%
  #filter(modN %in% ensemble_mods) %>%
ggplot( aes(x=monthN, y=brier_2sd, group=modN, color=modN))+
  geom_line()+
  theme_minimal()
ggplotly(p1)


## map brier scores
MDR_NEW <- st_read(dsn = "./Data/shapefiles/MDR_NEW_Boundaries_Final.shp")
MDR_NEW <- MDR_NEW %>%
  dplyr::mutate(District_province = paste( VARNAME,NAME_En, sep = " "))
MDR_NEW$VARNAME<- toupper(MDR_NEW$VARNAME)
MDR_NEW$NAME_En<- toupper(MDR_NEW$NAME_En)
MDR_NEW  <- MDR_NEW  %>%
  mutate(VARNAME = ifelse(VARNAME == 'CHAU THANH' & NAME_En == "AN GIANG",
                          "CHAU THANH AN GIANG",
                          ifelse(VARNAME == 'CHAU THANH' & NAME_En == "BEN TRE",
                                 "CHAU THANH BEN TRE",
                                 ifelse(VARNAME == 'CHAU THANH' & NAME_En == "CA MAU",
                                        "CHAU THANH CA MAU",
                                        ifelse(VARNAME == 'CHAU THANH' & NAME_En == "DONG THAP",
                                               "CHAU THANH DONG THAP",
                                               ifelse(VARNAME == 'CHAU THANH' & NAME_En == "HAU GIANG",
                                                      "CHAU THANH HAU GIANG",
                                                      ifelse(VARNAME == 'CHAU THANH' & NAME_En == "LONG AN",
                                                             "CHAU THANH LONG AN",
                                                             ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TIEN GIANG",
                                                                    "CHAU THANH TIEN GIANG",
                                                                    ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TRA VINH",
                                                                           "CHAU THANH TRA VINH",
                                                                           ifelse(VARNAME == 'PHU TAN' & NAME_En == "CAM MAU",
                                                                                  "PHU TAN CA MAU",
                                                                                  ifelse(VARNAME == 'PHU TAN' & NAME_En == "AN GIANG",
                                                                                         "PHU TAN AN GIANG",
                                                                                         as.character(VARNAME)
                                                                                  )
                                                                           )
                                                                    )))))))))
district_brier <- b1 %>%
  group_by(district,modN) %>%
  summarize(brier_2sd=mean(brier_2sd)) %>%
  ungroup() %>%
  filter(modN=='mod61_') 

p3 <- MDR_NEW %>%
  inner_join(district_brier,by=c('VARNAME'='district')) %>%
   ggplot() +
  geom_sf(aes(fill = brier_2sd)) +
  scale_fill_viridis()+
 # scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  theme_minimal()

p3

district_chars <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  group_by(district) %>%
  summarize(cases=sum(m_DHF_cases), 
            pop=sum(pop),cluster=mean(cluster),
            avg_max_daily_temp=mean(avg_max_daily_temp),
            monthly_cum_ppt=mean(monthly_cum_ppt),
            Urbanization_Rate=mean(Urbanization_Rate), 
            Poverty_Rate=mean(Poverty_Rate), 
            NetImmigration_Rate=mean(NetImmigration_Rate)
            
            ) %>%
  mutate(inc=cases/pop*100000)
  
  
brier_cor <- district_brier %>%
  left_join(district_chars, by='district')

#no clear relationship between brier score and covariates
brier_cor %>% 
  dplyr::select(-district, -modN, -cases, -pop)%>%
  mutate(cluster=as.factor(cluster)) %>%
ggpairs(., aes(color=cluster, alpha = 0.4))


  # ds <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
# 
# View(ds %>% group_by(date) %>% summarize(N=n()))


#ggplotly(p2)  



## DISTRICT-LEVEL PLOTS--make gifs showing observed and expected

gif.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter(horizon==2 & modN %in% ensemble_mods)%>%
  mutate(date2=date)

all.districts <- unique(out$district)

plot.dist.fun <- function(district.select){
  
  plot.df <- gif.ds %>%
    filter(district %in% district.select) %>%
    ungroup() 
  
  p2 <-   plot.df%>%  
    ggplot() +
    geom_line(aes(x=date, y=m_DHF_cases, group=modN), size=2, color='black') +
    theme_classic()+
    ylim(0,NA)+
    geom_ribbon(aes(x=date2, ymin=pred_lcl, ymax=pred_ucl, fill=modN, group=modN),linetype=0,alpha=0.1)+
    geom_line(aes(x=date2, y=pred_mean, group=modN, color=modN, linetype=modN), alpha=0.5,color='red')+
    ggtitle('Ensemble')+
    geom_hline(yintercept=43.75, col='gray', lty=2)+
    ggtitle(district.select) +
    transition_manual(as.Date(date),cumulative =T)
  
  animate(p2, fps=5, nframes=length(unique(plot.df$date)), renderer = gifski_renderer(),end_pause = 20)
  
  anim_save(paste0("./Data/obs_exp_gifs/obs_exp_",district.select, ".gif"))
  
}

all.plots <- lapply(all.districts[1:20],plot.dist.fun)



########
#Maps of flagged outbreaks

pp.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==2 & modN %in% ensemble_mods ) %>% #RESTRICTS TO THE SELECTED ENSEMBLE
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,district) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean),threshold_quant=threshold_quant) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, district) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count) #summarize across the different models to get date and district-specific estimate
  )
obs_epidemics<- obs_epidemics %>% filter(date >= as.Date("2012-03-01") & date <= as.Date("2016-12-01"))
q <- inner_join(pp.ds, obs_epidemics, by = c("date" = "date", "district" = "district"))
df <- q %>%
  mutate(epidemic_flag_ensamble_possion = ifelse(ensemble_dist_wgt > threshold_poisson, 1, 0),
         epidemic_flag_ensamble_nb = ifelse(ensemble_dist_wgt > threshold_nb, 1, 0),
         epidemic_flag_ensamble_fixed = ifelse(ensemble_dist_wgt > threshold, 1, 0),
         epidemic_flag_ensamble_quant = ifelse(ensemble_dist_wgt > threshold_quant, 1, 0))

map_plot_fun <- function(date.select="2014-07-01"){
    ##Show the plot for one vintage date
    ts_ds <-  df[df$date<=date.select,]
    df_one<- df[df$date==date.select,]
    ##Pull the shape file
    
    MDR_NEW <- st_read(dsn = "./Data/shapefiles/MDR_NEW_Boundaries_Final.shp")
    MDR_NEW <- MDR_NEW %>%
      dplyr::mutate(District_province = paste( VARNAME,NAME_En, sep = " "))
    MDR_NEW$VARNAME<- toupper(MDR_NEW$VARNAME)
    MDR_NEW$NAME_En<- toupper(MDR_NEW$NAME_En)
    MDR_NEW  <- MDR_NEW  %>%
      mutate(VARNAME = ifelse(VARNAME == 'CHAU THANH' & NAME_En == "AN GIANG",
                              "CHAU THANH AN GIANG",
                              ifelse(VARNAME == 'CHAU THANH' & NAME_En == "BEN TRE",
                                     "CHAU THANH BEN TRE",
                                     ifelse(VARNAME == 'CHAU THANH' & NAME_En == "CA MAU",
                                            "CHAU THANH CA MAU",
                                            ifelse(VARNAME == 'CHAU THANH' & NAME_En == "DONG THAP",
                                                   "CHAU THANH DONG THAP",
                                                   ifelse(VARNAME == 'CHAU THANH' & NAME_En == "HAU GIANG",
                                                          "CHAU THANH HAU GIANG",
                                                          ifelse(VARNAME == 'CHAU THANH' & NAME_En == "LONG AN",
                                                                 "CHAU THANH LONG AN",
                                                                 ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TIEN GIANG",
                                                                        "CHAU THANH TIEN GIANG",
                                                                        ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TRA VINH",
                                                                               "CHAU THANH TRA VINH",
                                                                               ifelse(VARNAME == 'PHU TAN' & NAME_En == "CAM MAU",
                                                                                      "PHU TAN CA MAU",
                                                                                      ifelse(VARNAME == 'PHU TAN' & NAME_En == "AN GIANG",
                                                                                             "PHU TAN AN GIANG",
                                                                                             as.character(VARNAME)
                                                                                      )
                                                                               )
                                                                        )))))))))
    q1<- inner_join(MDR_NEW,df_one,by=c('VARNAME'='district'))
    
    
    q1_a <- ggplot(data = q1) +
      geom_sf(aes(fill = as.factor(epidemic_flag_ensamble_possion))) +
      scale_fill_manual(values = c("0" = "white", "1" = "red")) +
      theme_minimal()
    q1_b<- ggplot(data = q1) +
      geom_sf(aes(fill = as.factor(epidemic_flag_ensamble_nb))) +
      scale_fill_manual(values = c("0" = "white", "1" = "red")) +
      theme_minimal()
    q1_c<- ggplot(data = q1) +
      geom_sf(aes(fill = as.factor(epidemic_flag_ensamble_quant))) +      theme_minimal()
    q1_d<- ggplot(data = q1) +
      geom_sf(aes(fill = as.factor(epidemic_flag_ensamble_fixed)) )+
      scale_fill_manual(values = c("0" = "white", "1" = "red")) +
      theme_minimal()
    
    tot_cases <- obs_case %>% group_by(date) %>%
      summarize(m_DHF_cases=sum(m_DHF_cases))
    
    p1.ds <- out_1a %>%
      filter(modN %in% ensemble_mods) %>%
      left_join(miss.dates, by=c('date','horizon')) %>%
      filter(miss_date==0 & exclude_miss_mod==0)  %>%
      filter( horizon==2 ) %>%
      full_join(obs_case, by=c('date','district')) %>%
      dplyr::select(-form) %>%
      group_by(modN,date,vintage_date,horizon) %>%
      dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
      mutate(month=month(date)) %>%
      left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
      ungroup() %>%
      group_by(date, vintage_date,horizon) %>%
      mutate(sum_wgts=sum(w_i2)) %>%
      dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count),
                       m_DHF_cases=mean(m_DHF_cases),pop=mean(pop)) %>%
      ungroup() %>%
      arrange( date) %>%
      filter(date<=date.select) %>%
      mutate(horizon=max(horizon, na.rm=T),
             vintage_date = as.Date(date.select) %m-% months(horizon),
        cases_hold = if_else(date>vintage_date, NA_real_, m_DHF_cases),
        cases_hold_dot = if_else(date==vintage_date, m_DHF_cases, NA_real_),
        ensemble_month_dot = if_else(date==date.select, ensemble_month, NA_real_)) %>%
      filter(date>='2010-01-01')

    p1b <- p1.ds %>%
      ggplot()+
      geom_line(aes(x=date, y=cases_hold)) +
      theme_classic()+
      ylim(0,NA)+
      geom_line(aes(x=date, y=ensemble_month), alpha=0.5 , col='red')+
      geom_point(aes(x=date, y=ensemble_month_dot), alpha=1 , col='red', cex=2)+
            geom_point(aes(x=date, y=cases_hold_dot), alpha=1 , col='black', cex=2)+
          ggtitle("Predictions by Date")

    #outputs
    p1b    
    ggarrange(p1b,q1_a, q1_b, q1_c, q1_d, nrow = 3, ncol = 2)
    
    
}
