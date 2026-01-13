library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(pbapply)
#inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
#library(ggmap) # plotting shapefiles 
library(lattice)  # Load the lattice package if you are using lattice graphics
library(stringr)
library(janitor)
library(surveillance)
#library(roll)
library(lubridate)
library(INLA)

source('./R/99_helper_funcs.R')
source('./R/99_define_inla_spacetime_mods.R')
source('./R/01_fun_inla_spacetime.R')
source('./R/02_fun_hhh4.R')
source('./R/03_fun_lag_district_pca.R')

d2<-  readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3_add_rows_full_pop.rds')

# d2 <- readRDS('./Data/CONFIDENTIAL/cleaned_data.rds')
# #cleaned in format_input_data.R
# d2 <- readRDS('./Data/CONFIDENTIAL/Updated_merged_data.rds')  %>%
#   arrange(district, date) %>%
#   group_by(district) %>%
#   mutate(   cumsum_cases_12m =  roll::roll_sum(m_DHF_cases,12, min_obs=1), #partial backward moving sum
#             cumsum_pop_12m =  roll::roll_sum(pop,12, min_obs=1), #partial backward moving sum
#             cum_inc_12m = (cumsum_cases_12m+1)/cumsum_pop_12m*100000,
#             cumsum_cases_24m =  roll::roll_sum(m_DHF_cases,24, min_obs=1), #partial backward moving sum
#             cumsum_pop_24m =  roll::roll_sum(pop,24, min_obs=1), #partial backward moving sum
#             cum_inc_24m = (cumsum_cases_24m+1)/cumsum_pop_24m*100000,
#             cumsum_cases_36m =  roll::roll_sum(m_DHF_cases,36, min_obs=1), #partial backward moving sum
#             cumsum_pop_36m =  roll::roll_sum(pop,36, min_obs=1), #partial backward moving sum
#             cum_inc_36m = (cumsum_cases_36m+1)/cumsum_pop_36m*100000
#   ) %>%
#     ungroup() %>%
#     mutate(log_cum_inc_12m=scale(log(cum_inc_12m)),
#            log_cum_inc_24m=scale(log(cum_inc_24m)),
#            log_cum_inc_36m=scale(log(cum_inc_36m)),
#            lag2_log_cum_inc_12m=lag(log_cum_inc_12m,2),
#            lag2_log_cum_inc_24m=lag(log_cum_inc_24m,2),
#            lag2_log_cum_inc_36m=lag(log(cum_inc_36m,2))
# 
#     )

d<- d2[ , c("No..DEN1", "No..DEN2", "No..DEN3", "No..DEN4")]


d$podem <- NA  # Initialize the column with NA values

# Loop through rows of the dataframe
# Extract only the required columns
d <- d2[, c("No..DEN1", "No..DEN2", "No..DEN3", "No..DEN4")]

# Initialize the "podem" column with NA values
d$podem <- NA  

# Loop through rows of the dataframe
for (i in 1:nrow(d)) {
  row <- d[i, ]  # Extract the current row
  
  # Check if the row contains only zeros (excluding NA values)
  if (all(row[!is.na(row)] == 0)) {  
    # If all zeros, assign previous "podem" value or 2 if it's the first row
    if (i == 1) {
      d$podem[i] <- 2  
    } else {
      d$podem[i] <- d$podem[i - 1]  
    }
  } else {
    # If not all zeros, return the column number with the maximum value
    if (all(is.na(row[-length(row)]))) {  
      d$podem[i] <- NA  # If all values are NA, keep NA
    } else {
      max_col <- which.max(replace(row[-length(row)], is.na(row[-length(row)]), -Inf))  
      d$podem[i] <- max_col
    }
  }
}



d2<- d2%>%
  dplyr::mutate(prediomentent=as.factor(d$podem))



d2_filtered <- d2[is.na(d2$pop), ]

# Ensure d2 is sorted by Year, Month, District, and Province for correct indexing
d2 <- d2[order(d2$year, d2$month, d2$district, d2$province), ]

# Identify the rows with NA in 'pop' for the specified condition
na_rows <- which(is.na(d2$pop) & d2$year == 2022 & d2$district == "CAO LANH CITY"& d2$province ==  "DONG THAP")

# Loop through the identified rows and fill 'pop' from the corresponding month in 2021
for (i in na_rows) {
  # Find the corresponding row in 2021
  corresponding_row <- which(d2$year == 2021 & d2$month == d2$month[i] & 
                               d2$district == "CAO LANH CITY" & d2$province == "DONG THAP")
  
  # If a corresponding row exists, fill the NA value with the 'pop' from 2021
  if (length(corresponding_row) == 1) {
    d2$pop[i] <- d2$pop[corresponding_row]
  }
}

###
# Ensure d2 is sorted by Year, Month, District, and Province for correct indexing
d2 <- d2[order(d2$year, d2$month, d2$district, d2$province), ]

# Identify the rows with NA in 'pop' for the specified condition
na_rows <- which(is.na(d2$Population_density) & d2$year == 2022 & d2$district == "CAO LANH CITY"& d2$province ==  "DONG THAP")

# Loop through the identified rows and fill 'pop' from the corresponding month in 2021
for (i in na_rows) {
  # Find the corresponding row in 2021
  corresponding_row <- which(d2$year == 2021 & d2$month == d2$month[i] & 
                               d2$district == "CAO LANH CITY" & d2$province == "DONG THAP")
  
  # If a corresponding row exists, fill the NA value with the 'pop' from 2021
  if (length(corresponding_row) == 1) {
    d2$Population_density[i] <- d2$Population_density[corresponding_row]
  }
}
#########

MDR_NEW <- readRDS( './Data/MDR_NEW.rds')

spat_IDS <- readRDS( "./Data/spatial_IDS.rds")

neighb <- poly2nb(st_make_valid(MDR_NEW), queen = T, snap = sqrt(0.001))

nb2INLA("MDR.graph", neighb)

MDR.adj <- paste(getwd(), "/MDR.graph", sep = "")



date.test2 <- seq.Date(from=as.Date('2022-01-01') ,to=as.Date('2022-12-01') , by='month')

all.districts <- unique(d2$district)

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

# hyperpriors for model grouping (iid / ar1) if used
# group.control.iid = list(model='iid', hyper = list(prec = list(prior='pc.prec',param=c(1, 0.01))))
# group.control.ar1 = list(model='ar1', hyper = list(theta1 = list(prior='pc.prec', param=c(1, 0.01)), rho = list(prior='pc.cor0', param = c(0.5, 0.75))))

# rw1/rw2 model: three levels of constraint on precision parameter 
# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.01))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior='pc.prec', param=c(1, 0.01))) # weaker (suggested INLA default) 
hyper4.rw = list(prec = list(prior='pc.prec', param=c(2, 0.01))) # weakest; sd can be quite wide 





# Load required libraries
# library(ggplot2)
# library(dplyr)
# 
# d2 <- d2 %>%
#   mutate(year = as.numeric(as.character(year)))
# 
# # Filter and prepare the data
# mdr_data <- d2 %>%
#   filter( year >='2012') %>%
#   group_by(year, month) %>%
#   summarise(total_cases = sum(m_DHF_cases, na.rm = TRUE), .groups = 'drop')
# 
# # Plot
# ggplot(mdr_data, aes(x = month, y = total_cases, color = as.factor(year))) +
#   geom_line(size = 1) +
#   scale_x_continuous(breaks = 1:12, labels = month.abb) +
#   labs(
#     title = "Monthly Dengue Incidence Curves (2012-2022)",
#     x = "Month",
#     y = "Total Dengue Cases",
#     color = "Year"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(size = 10),
#     axis.text = element_text(size = 10),
#     axis.title = element_text(size = 12)
#   )
