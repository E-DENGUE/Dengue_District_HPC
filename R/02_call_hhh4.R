source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

mods <- c('hhh4_np','hhh4_power','hhh4_basic','hhh4_power_precip_temp','hhh4_power_precip_temp_endmc1',
          'hhh4_power_precip_temp_endmc2','hhh4_power_cum_lag24','hhh4_power_lag12','hhh4_power_precip_temp_dist')
#mods <- c('hhh4_power_precip_temp')

#i=1 #108 dates total
#k=1 #10 models

mod1 <- hhh4_mod(vintage_date = date.test2[j], modN=k,max_horizon=2) 


# for(j in 1:length(date.test2)){
#   for(k in 1:length(mods)){
#     print(j)
#     print(k)
#     mod1 <- hhh4_mod(vintage_date = date.test2[j], modN=k,max_horizon=2) 
#     
#   }
#   
# }