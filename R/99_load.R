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

source('./R/99_helper_funcs.R')
source('./R/99_define_inla_spacetime_mods.R')
source('./R/01_fun_inla_spacetime.R')
source('./R/02_fun_hhh4.R')
source('./R/03_fun_lag_district_pca.R')

#d2 <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
#cleaned in format_input_data.R
d2 <- readRDS('./Data/CONFIDENTIAL/merged_data.rds') %>%
  rename(District_province = prov_dis) %>%
  arrange(district, date) %>%
  group_by(district) %>%
  mutate(   cumsum_cases_12m =  roll::roll_sum(m_DHF_cases,12, min_obs=1), #partial backward moving sum
            cumsum_pop_12m =  roll::roll_sum(pop,12, min_obs=1), #partial backward moving sum
            cum_inc_12m = (cumsum_cases_12m+1)/cumsum_pop_12m*100000,
            cumsum_cases_24m =  roll::roll_sum(m_DHF_cases,24, min_obs=1), #partial backward moving sum
            cumsum_pop_24m =  roll::roll_sum(pop,24, min_obs=1), #partial backward moving sum
            cum_inc_24m = (cumsum_cases_24m+1)/cumsum_pop_24m*100000,
            cumsum_cases_36m =  roll::roll_sum(m_DHF_cases,36, min_obs=1), #partial backward moving sum
            cumsum_pop_36m =  roll::roll_sum(pop,36, min_obs=1), #partial backward moving sum
            cum_inc_36m = (cumsum_cases_36m+1)/cumsum_pop_36m*100000
  ) %>%
    ungroup() %>%
    mutate(log_cum_inc_12m=scale(log(cum_inc_12m)),
           log_cum_inc_24m=scale(log(cum_inc_24m)),
           log_cum_inc_36m=scale(log(cum_inc_36m)),
           lag2_log_cum_inc_12m=lag(log_cum_inc_12m,2),
           lag2_log_cum_inc_24m=lag(log_cum_inc_24m,2),
           lag2_log_cum_inc_36m=lag(log(cum_inc_36m,2))
  
    )

d<- d2[ , c("No..DEN1", "No..DEN2", "No..DEN3", "No..DEN4")]


d$podem <- NA  # Initialize the column with NA values

# Loop through rows of the dataframe
for (i in 1:nrow(d)) {
  row <- d[i, ]  # Extract the current row
  
  # Check if all values in the row are zeros
  if (all(row[-length(row)] == 0)) {  # Exclude the last column (podem column) from the check
    # If all zeros, return the value from the previous row's "podem" column
    if (i == 1) {
      d$podem[i] <- 2  # For the first row, assign 2
    } else {
      d$podem[i] <- d$podem[i - 1]  # For subsequent rows, assign the previous row's value
    }
  } else {
    # If not all zeros, return the column number with the maximum value
    max_col <- which.max(row[-length(row)])  # Exclude the last column (podem column)
    d$podem[i] <- max_col
  }
}

d2<- d2%>% 
  dplyr::mutate(prediomentent=as.factor(d$podem))



MDR_NEW <- readRDS( "./Data/MDR_NEW.rds")

spat_IDS <- readRDS( "./Data/spatial_IDS.rds")


# Set the file path for the adjacency graph file
MDR.adj <- paste(getwd(), "/Data/MDR.graph", sep = "")

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

