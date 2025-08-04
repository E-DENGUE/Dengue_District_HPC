source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  

#which models to run?
all.mods <- list()

modN_extract = as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1,2])

mod1 <- cusum_mod(vintage_date = date.test2[j],  modN=modN_extract ) 