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
library(lubridate)

source('./R/99_helper_funcs.R')
source('./R/99_define_inla_spacetime_mods.R')
source('./R/01_fun_inla_spacetime.R')
source('./R/02_fun_hhh4.R')
source('./R/03_fun_lag_district_pca.R')

#cleaned in 00_format_input_data.R
d2 <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  rename(District_province = prov_dis) %>%
  arrange(district, date) 
  
MDR_NEW <- readRDS( "./Data/MDR_NEW.rds")

spat_IDS <- readRDS( "./Data/spatial_IDS.rds")

neighb <- poly2nb(st_make_valid(MDR_NEW), queen = T, snap = sqrt(0.001))

nb2INLA("./Data/MDR.graph", neighb)

# p <- st_make_valid(MDR_NEW)
#xx <- poly2nb(p, queen = T, snap = sqrt(0.001))
#coords <- st_coordinates(st_centroid(st_geometry(p))) # this seems to miss some polygons
#plot(st_geometry(p), border="grey")
#plot(xx, coords, pch = 19, cex = 0.4, add=TRUE)

# Set the file path for the adjacency graph file
MDR.adj <- paste(getwd(), "./Data/MDR.graph", sep = "")

date.test2 <- seq.Date(from=as.Date('2012-01-01') ,to=as.Date('2016-12-01') , by='month')

all.districts <- unique(d2$district)

##Priors from Gibb ms 
# iid model 
hyper.iid = list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# ar1 model
hyper.ar1 = list(theta1 = list(prior='pc.prec', param=c(0.5, 0.01)),
                 rho = list(prior='pc.cor0', param = c(0.5, 0.75)))

# bym model
hyper.bym = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                 theta2 = list(prior="pc.prec", param=c(1, 0.01)))

# bym2 model
# probability of SD of theta1 > 1 = 0.01
hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.5)))

# hyperpriors for model grouping (iid / ar1) if used
# group.control.iid = list(model='iid', hyper = list(prec = list(prior='pc.prec',param=c(1, 0.01))))
# group.control.ar1 = list(model='ar1', hyper = list(theta1 = list(prior='pc.prec', param=c(1, 0.01)), rho = list(prior='pc.cor0', param = c(0.5, 0.75))))

# rw1/rw2 model: three levels of constraint on precision parameter 
# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.01))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior='pc.prec', param=c(1, 0.01))) # weaker (suggested INLA default) 
hyper4.rw = list(prec = list(prior='pc.prec', param=c(2, 0.01))) # weakest; sd can be quite wide 

