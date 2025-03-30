source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])
i <- as.numeric(args[3])

mods <- c('PC_lags_weather','PC_lags','PC_weather')

#j=1 #108 dates total
i=2 #10 models

mod1 <- lag_district_pca(vintage_date = date.test2[j], district.select=all.districts[k],modN=i ) 
