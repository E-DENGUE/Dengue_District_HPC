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


out.slim<-  bind_rows(ds.list) %>%
  dplyr::select(date, modN, eval.date, district, province, m_DHF_cases,m_DHF_cases_hold, pop) %>%
 saveRDS(., "./cleaned_scores/all_crps.rds")

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

out <- readRDS( "./cleaned_scores/all_crps.rds")
out.slim <- out %>%
  dplyr::select(date, modN, eval.date, district, province, m_DHF_cases,m_DHF_cases_hold, pop)
saveRDS( out.slim,"./cleaned_scores/all_crps.rds")


miss.dates <- out %>% group_by(date) %>%   summarize(N_mods=n(),N_cases=sum(m_DHF_cases )) %>%
  ungroup() %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 ))

#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  geom_point(aes(x=date, y=N_cases, color=miss_date))
