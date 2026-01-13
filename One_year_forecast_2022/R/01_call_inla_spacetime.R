source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

#i=1 #108 dates total
#k=1 #10 models

#which models to run?
all.mods <- list()

for (i in 1:61) {
  all.mods[[paste0("mod", i)]] <- get(paste0("mod", i))
}


#all.mods <- list('mod10' = mod10,'mod20' = mod20,'mod21' = mod21,'mod22' = mod22,'mod23' = mod23,'mod24' = mod24)


for(j in c(12)){
  for (k in c(25)){
modN_extract = as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1,2])


mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract ) 
  }
}

