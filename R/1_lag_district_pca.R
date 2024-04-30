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
library(roll)
library(lubridate)


source('./R/99_load.R')

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])
i <- as.numeric(args[3])

mods <- c('PC_lags_weather','PC_lags','PC_weather')

#j=1 #108 dates total
#i=3 #10 models

mod1 <- lag_district_pca(date.test.in = date.test2[j], district.select=all.districts[k],modN=i ) 