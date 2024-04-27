library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
#inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
library(ggmap) # plotting shapefiles 
library(lattice)  # Load the lattice package if you are using lattice graphics
library(stringr)
library(janitor)
library(surveillance)

source('./R/load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

#i=1 #108 dates total
#k=1 #10 models

#which models to run?
all.mods <- list('mod28'=mod28,'mod29'=mod29, 'mod30'=mod30,'mod31'=mod31,'mod32'=mod32,  'mod33'=mod33,  'mod34'=mod34,  'mod35'=mod35,  'mod36'=mod36,  'mod37'=mod37,  'mod38'=mod38)

modN_extract = as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1,2])

mod1 <- inla_spacetime_mod(date.test.in = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract, type4mod=grepl("type4",names(all.mods)[k]) ) 