library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)  # Includes dplyr, ggplot2, tidyr, readr, etc.
library(zoo)
library(pbapply)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
library(lattice)
library(stringr)
library(janitor)
library(surveillance)
library(lubridate)
library(INLA)


source('./R/99_helper_funcs.R')
source('./R/99_define_inla_spacetime_mods.R')
source('./R/01_fun_inla_spacetime.R')
source('./R/02_fun_hhh4.R')
source('./R/03_fun_lag_district_pca.R')

##read the file from "Input" folder
d2<-  readRDS('./Data/Full_data_set_with_covariates_and_lags.rds')


MDR_NEW <- readRDS( './Data/MDR_NEW.rds')
MDR_NEW <- MDR_NEW %>%
  dplyr::filter(fcode != "ED_KIEN_GIANG_KIEN_HAI_DISTRICT",
                fcode != "ED_KIEN_GIANG_PHU_QUOC_CITY")

spat_IDS <- readRDS( "./Model/Data/spatial_IDS.rds")

neighb <- poly2nb(st_make_valid(MDR_NEW), queen = T, snap = sqrt(0.001))

nb2INLA("MDR.graph", neighb)

MDR.adj <- paste(getwd(), "/MDR.graph", sep = "")


#date.test2 <- seq.Date(from=as.Date(max(d2$date)) %m-% months(5) ,to=as.Date(max(d2$date)) %m-% months(3) , by='month')

#date.test2 <- seq.Date(from=as.Date('2012-01-01')  ,to=as.Date('2025-04-01') , by='month')

date.test2 <- as.Date(max(d2$date%m-% months(3)))


all.fcodes <- unique(d2$fcode)

hyper.besag =   hyper = list(prec = list(prior = "loggamma",
                                          param = c(1, 1), initial = 0.01))

hyper1 = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
              prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))
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


# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.01))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior='pc.prec', param=c(1, 0.01))) # weaker (suggested INLA default) 
hyper4.rw = list(prec = list(prior='pc.prec', param=c(2, 0.01))) # weakest; sd can be quite wide 


