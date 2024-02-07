library(parallel)
library(stats)

source('./R/load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

#i=1 #108 dates total
#k=1 #10 models

mod1 <- all_district_fwd1(date.test.in = date.test2[j], formula1 = all.mods[[k]], modN=as.numeric(gsub("mod","",names(all.mods)[k]) )   )

