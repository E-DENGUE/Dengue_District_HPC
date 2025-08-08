source('./Model/R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

mods <- c('hhh4_np','hhh4_power','hhh4_basic','hhh4_power_precip_temp','hhh4_power_precip_temp_endmc1','hhh4_power_precip_temp_endmc2')
mods <- c('hhh4_power_precip_temp') ##We have just this model in the ensemble

k=1

#j=1

for (j in 1:length(date.test2)){
 mod1 <- hhh4_mod(vintage_date = date.test2[j], modN = k, max_horizon = 3) 

}
