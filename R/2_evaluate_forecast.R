##In console:
# salloc
# module load R/4.2.0-foss-2020b
# R

#setwd("~/project/dengue_test/Cluster_DW")


library(dplyr)
library(parallel)


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


scores <-  bind_rows(ds.list) %>%
    group_by(modN) %>%
    summarize(score_sum=mean(crps1) , n.obs=n())


scores_filtered <-  bind_rows(ds.list) %>%
     group_by( district, date) %>%
mutate(N_obs=n() ) %>%
ungroup() %>%
filter(N_obs==max(N_obs)) %>%
group_by(modN) %>%
     summarize(score_sum=mean(crps1) , n.obs=n())
     
     scores_filtered <-  bind_rows(ds.list) %>%
     group_by( district, date) %>%
mutate(N_obs=n() ) %>%
ungroup() %>%
filter(N_obs==max(N_obs)) %>%
group_by(horizon,modN) %>%
     summarize(score_sum=mean(crps1) , n.obs=n()) %>%
     ungroup() %>%
    arrange(horizon,score_sum)

print(scores_filtered, n=100)