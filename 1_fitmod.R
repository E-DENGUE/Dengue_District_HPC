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

source('./R/load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

#i=1 #108 dates total
#k=1 #10 models
modN_extract = as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1,2])

mod1 <- all_district_fwd1(date.test.in = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract  ) 