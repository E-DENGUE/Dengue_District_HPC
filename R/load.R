library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(reshape2)
library(pbapply)
library(INLA)
inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)


source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
source('./R/all_district_fwd1.R')
source('./R/scoring_func.R')

######## Load data

#data from other parts of Vietnam
 # p1 <- read_excel('./Data/CONFIDENTIAL/Dengue districts 1998-2019.xlsx', skip=1) %>%
 #   reshape2::melt(., id.vars=c('Province','District')) %>%
 #   rename(date=variable, dengue=value) %>%
 #   mutate( date=as.numeric(as.character(date)),
 #           date= as.Date(date, origin = "1899-12-30"),
 #           date=floor_date(date, 'month'),
 #           dengue=as.numeric(dengue),
 #           Province=tolower(Province)
 # ) %>%
 #   filter(Province %in% c("ha noi"  ,  "dak lak" ,  "khanh hoa" ,"dong nai"    )) %>%
 #   group_by(Province, date) %>%
 #   summarize( dengue=sum(dengue, na.rm=T)) %>%
 #   ungroup() %>%
 #   filter(date>='2001-01-01' & date<='2017-12-01')
 # 
 # ggplot(p1, aes(x=date, y=dengue, group=Province, color=Province))+geom_line() +facet_wrap(~Province, scales='free')

d1 <- readRDS('./Data/CONFIDENTIAL/full_climate_model.rds')

d2 <- d1 %>%
   dplyr::select(-geometry) %>%
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

#All models

#only models 1,2,3,6,7,8,13 successfully ran

###############best model based on CRPS (full posterior) ############
mod1 <- 'm_DHF_cases~   lag_y + 
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         

#same as mod 9 without AR1
mod2 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#building off of model 9, adds rainfall
mod3 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         

#building off of model 9, allow AR1 to vary by district
mod4 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         

#building off of model 9, allow AR1 to vary by district AND add rainfall
mod5 <- 'm_DHF_cases~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         

## try adding different combos of climate vars
mod6 <- 'm_DHF_cases_hold ~   lag_y + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod7 <- 'm_DHF_cases_hold ~   lag_y +  lag1_avg_min_daily_temp +lag2_avg_min_daily_temp +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod8 <- 'm_DHF_cases_hold ~   lag_y +  lag1_avg_max_daily_temp +lag2_avg_max_daily_temp +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

## district AR(1) without  lagged cases           
mod9 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                        
                         
 ## district AR(1) without  lagged cases    +rain       
mod10 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         
                         
 ## district AR(1) without  lagged cases    +temp       
mod11 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                         
                         
 ## district AR(1) without  lagged cases    +temp   +rain    
mod12 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

                         
 ## district AR(1) without  lagged cases    +all 
mod13 <- 'm_DHF_cases_hold~    lag_y + 
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        lag1_avg_max_daily_temp +lag2_avg_max_daily_temp + lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 
#Same as mod 1 with AR1 instead of RW1                         
   mod14 <- 'm_DHF_cases~   lag_y + 
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 

all.mods <- list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10, mod11, mod12, mod13, mod14)
