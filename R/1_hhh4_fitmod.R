library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
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
library(roll)
library(lubridate)


source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

 #mods <- c('hhh4_np','hhh4_power','hhh4_basic','hhh4_power_precip_temp','hhh4_power_precip_temp_endmc1','hhh4_power_precip_temp_endmc2')
mods <- c('hhh4_power_precip_temp_dist')

#i=1 #108 dates total
#k=1 #10 models

mod1 <- hhh4_mod(date.test.in = date.test2[j], modN=k,max_horizon=2) 
