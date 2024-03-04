##In console:
# salloc
# module load R/4.2.0-foss-2020b
# R

#setwd("~/project/dengue_test/Cluster_DW")


library(dplyr)
library(parallel)
library(ggplot2)

N_cores = detectCores()

file.names <- list.files('./Results')

ds.list <- mclapply(file.names,function(X){
  
    d1 <- readRDS(file=file.path(paste0('./Results/',X)))
    
    modN <-  sub("^(.*?)_.*$", "\\1", X)
    
    date_pattern <- "\\d{4}-\\d{2}-\\d{2}"

    # Extract the date from the string using gsub
    date <- regmatches(X, regexpr(date_pattern, X))

    out.list <- cbind.data.frame('modN'=modN,'eval.date'=date, d1$scores)
  return(out.list)
},  mc.cores=N_cores)

#out <- readRDS("./cleaned_scores/all_crps.rds")%>% dplyr::select(date, modN, eval.date,horizon, district, province, m_DHF_cases,m_DHF_cases_hold, pop, crps1) %>%
# saveRDS(., "./cleaned_scores/all_crps_slim.rds")

out.slim<-  bind_rows(ds.list) %>%
  dplyr::select(date, modN, eval.date,horizon, district, province, m_DHF_cases,m_DHF_cases_hold, pop, crps1) %>%
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
  filter(!(modN %in% c('mod31','mod32'))) %>%
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
  filter(miss_date==0 & !(modN %in% c('mod31','mod32'))) %>%
  group_by(horizon, modN) %>%
  summarize(crps1 = mean(crps1), N=n() ) %>%
  arrange(horizon, crps1)
out2

#By calendar month
out3 <- out %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & !(modN %in% c('mod31','mod32'))) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN) %>%
  summarize(crps1 = mean(crps1), N=n() ) %>%
  arrange(horizon,month, crps1)
View(out3)



# miss_pattern <- out %>% 
#   group_by(date, modN, horizon) %>%
#   summarize(N=n()) %>%
#   reshape2::dcast(., date + horizon~modN) %>%
#   arrange(horizon, date)





# ds <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
# 
# View(ds %>% group_by(date) %>% summarize(N=n()))
