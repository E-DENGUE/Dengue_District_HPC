library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
library(MASS)
library(scoringutils)


source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
source('./R/all_district_fwd1.R')
source('./R/scoring_func.R')

######## Load data
d1 <- readRDS('./Data/CONFIDENTIAL/full_climate_model.rds')

d2 <- d1 %>%
  mutate(date = paste(year, month, '01', sept='-'),
         year = as_factor(year)) %>%
  filter(VARNAME_2 != "Kien Hai", 
         VARNAME_2 != "Phu Quoc") %>%
  distinct(year, month, VARNAME_2, NAME_1, ENGTYPE_2, .keep_all = T) %>%
  arrange(month, year, VARNAME_2)%>%
  ungroup() %>%
  dplyr::select(year, month, district,m_DHF_cases,pop,avg_daily_temp,mean_ppt, avg_min_daily_temp, avg_max_daily_temp,avg_daily_humid, monthly_cum_ppt,
                mean_daily_temp,avg_min_daily_temp,avg_max_daily_temp,mean_max_temp,mean_min_temp,mean_humid) %>%
  ungroup() %>%
  arrange(district, year, month) %>%
  group_by(district) %>%
  mutate(date= as.Date(paste(year,month, '01',sep='-'), '%Y-%m-%d'),
         m_DHF_cases = if_else(!is.na(pop) & is.na(m_DHF_cases),0, m_DHF_cases ) ,
         first_date=min(date),
         last_date =max(date),
          ) %>%
  ungroup() %>%
  filter(!is.na(district) &first_date==as.Date('2001-01-01') & last_date=='2018-12-01')  #filter out regions with partial time series
  

rain1 <- deseasonalize_climate("monthly_cum_ppt") %>% rename(total_rainfall_ab = climate_aberration)
rain2 <- deseasonalize_climate("mean_ppt")  %>% rename(daily_rainfall_ab = climate_aberration)
temp1 <- deseasonalize_climate("mean_daily_temp")  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("avg_min_daily_temp")  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("avg_max_daily_temp")  %>% rename( min_ave_temp_ab = climate_aberration)
temp4 <- deseasonalize_climate("mean_max_temp")  %>% rename( max_abs_temp_abb = climate_aberration)
temp5 <- deseasonalize_climate("mean_min_temp")  %>% rename( min_abs_temp_abb= climate_aberration)
humid1 <- deseasonalize_climate("mean_humid")  %>% rename(min_humid_abb = climate_aberration)

d2 <- d2 %>%
  left_join(rain1, by=c('district', 'date')) %>%
  left_join(rain2, by=c('district', 'date')) %>%
  left_join(temp1, by=c('district', 'date')) %>%
  left_join(temp2, by=c('district', 'date')) %>%
  left_join(temp3, by=c('district', 'date')) %>%
  left_join(temp4, by=c('district', 'date')) %>%
  left_join(temp5, by=c('district', 'date')) %>%
  left_join(humid1, by=c('district', 'date')) %>%
  mutate( 
    #redefine the lag variables
    avg_daily_humid = as.vector(scale(avg_daily_humid)),
    lag1_avg_daily_humid = dplyr::lag(avg_daily_humid,1,default=NA),
    lag2_avg_daily_humid = dplyr::lag(avg_daily_humid,2,default=NA),
    lag3_avg_daily_humid = dplyr::lag(avg_daily_humid,3),
    lag4_avg_daily_humid = dplyr::lag(avg_daily_humid,4),
    lag5_avg_daily_humid = dplyr::lag(avg_daily_humid,5),
    lag6_avg_daily_humid = dplyr::lag(avg_daily_humid,6),
    
    avg_daily_temp= as.vector(scale(avg_daily_temp)),
    lag1_avg_daily_temp= dplyr::lag(avg_daily_temp,1),
    lag2_avg_daily_temp= dplyr::lag(avg_daily_temp,2),
    lag3_avg_daily_temp= dplyr::lag(avg_daily_temp,3),
    lag4_avg_daily_temp= dplyr::lag(avg_daily_temp,4),
    lag5_avg_daily_temp= dplyr::lag(avg_daily_temp,5),
    lag6_avg_daily_temp= dplyr::lag(avg_daily_temp,6),
    
    monthly_cum_ppt=as.vector(scale(monthly_cum_ppt)),
    lag1_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,1),
    lag2_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,2),
    lag3_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,3),
    lag4_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,4),
    lag5_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,5),
    lag6_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,6),
    
    avg_min_daily_temp=as.vector(scale(avg_min_daily_temp)),
    lag1_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,1),
    lag2_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,2),
    lag3_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,3),
    lag4_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,4),
    lag5_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,5),
    lag6_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,6),
    
    avg_max_daily_temp= as.vector(scale(avg_max_daily_temp)),
    lag1_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,1),
    lag2_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,2),
    lag3_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,3),
    lag4_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,4),
    lag5_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,5),
    lag6_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,6),
    
    lag1_total_rainfall_ab= dplyr::lag(total_rainfall_ab,1),
    lag2_total_rainfall_ab= dplyr::lag(total_rainfall_ab,2)
    
        
    )%>%
    filter(!is.na(lag6_monthly_cum_ppt) & first_date==as.Date('2001-01-01') & last_date=='2018-12-01')   #filter out regions with partial time series
  


# d1.agg <- d2 %>%
#   group_by(date) %>%
#   summarize(N=n())
# 
#   summarize( df_rates = m_DHF_cases/ pop*100000,
#     mean_dengue = mean(df_rates)) %>%
#   mutate(lag1_mean_dengue = lag(mean_dengue,1),
#          log_lag1_mean_dengue = log(lag1_mean_dengue +1 ))

# d2 <- d2 %>%
#   left_join(d1.agg, by='date')


### Run the models


#just test a few years
date.test2 <- seq.Date(from=as.Date('2012-01-01') ,to=as.Date('2018-12-01') , by='month')


#All models


###############best model based on CRPS (full posterior) ############
mod1 <- 'm_DHF_cases~   lag_y + 
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#same as mod 9 without AR1
mod2 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))
                         '

#building off of model 9, adds rainfall
mod3 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#building off of model 9, allow AR1 to vary by district
mod4 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#building off of model 9, allow AR1 to vary by district AND add rainfall
mod5 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

## try adding different combos of climate vars
mod6 <- 'm_DHF_cases_hold ~   lag_y + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'


mod7 <- 'm_DHF_cases_hold ~   lag_y +  lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'


mod8 <- 'm_DHF_cases_hold ~   lag_y +  lag1_Max_Average_Temperature +lag2_Max_Average_Temperature +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'

mod9 <- 'm_DHF_cases_hold~   lag_y +  log_lag1_mean_dengue +
                        f(districtID,model = "iid")+
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

mod10 <- 'm_DHF_cases_hold~   lag_y +  log_lag1_mean_dengue +
                        f(districtID,model = "iid")+
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
            '

## district AR(1) without  lagged cases           
mod11 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '
                         
 ## district AR(1) without  lagged cases    +rain       
mod12 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '
                         
 ## district AR(1) without  lagged cases    +temp       
mod13 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '     
                         
 ## district AR(1) without  lagged cases    +temp   +rain    
mod14 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(t, group=districtID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 
                         
 ## district AR(1) without  lagged cases    +all 
mod15 <- 'm_DHF_cases_hold~    lag_y + 
                        f(districtID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        lag1_Max_Average_Temperature +lag2_Max_Average_Temperature + lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 

all.mods <- list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10, mod11, mod12, mod13, mod14, mod15)
