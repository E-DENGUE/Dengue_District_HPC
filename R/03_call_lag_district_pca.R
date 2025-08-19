source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])
i <- as.numeric(args[3])

mods <- c('PC_lags_weather','PC_lags','PC_weather')
i=2        ##in the ensemble we have just 'PC_lags'


for(k in 1:112)
{for (j in 1:length(date.test2)){
mod1 <- lag_district_pca(vintage_date = date.test2[j], fcode.select=all.fcodes[k],modN=i ) 
}
}



