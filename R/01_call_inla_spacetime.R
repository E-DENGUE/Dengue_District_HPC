source('./R/99_load.R')
# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

j=1 #108 dates total
k=1 #10 models

#which models to run?
library(stringr)

all.mods <- list('mod1' = mod6, 'mod2' = mod60, 'mod3' = mod61)



modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])



mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract ) 

# for (j in 1:60){
#  for (k in 4:10) {
# mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract ) 
#  }
# }
