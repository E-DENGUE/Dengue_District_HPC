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

source('./R/load.R')


j=1
k=1

modN_extract = as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1,2])


date.test.in = date.test2[j]
modN=modN_extract

N_cores = detectCores() -1

c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop *100000,
         log_df_rate = log((m_DHF_cases +1)/ pop *100000) , 
         log_pop=log(pop/100000),
         year = year(date) ,
         m_DHF_cases_hold= ifelse( date>= (date.test.in[1]), NA_real_,
                                   m_DHF_cases),
         lag_y = lag(log_df_rate, 1),
         lag2_y = lag(log_df_rate, 2),
         max_allowed_lag = if_else(grepl('lag_y',formula1 )|grepl('lag1',formula1 ),1,2),
         horizon = if_else(date== (date.test.in[1]),1,
                           if_else(date== (date.test.in[1] %m+% months(1)),2, 0
                           )
         ),
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12),
         month=as.factor(month(date)),
         monthN=month(date),
         offset1 = pop/100000,
         #log_offset=log(pop/100000)
  ) %>%
  filter(date<= (date.test.in[1] %m+% months(1) ) & !is.na(lag2_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
  ungroup() %>%
  mutate(
    districtID2 = districtID,
    districtID3 = districtID,
    districtID4 = districtID,
    t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
    
    time_id1= t , 
    time_id2=t,
    time_id3= t,
    urban_dic = if_else(Urbanization_Rate>=40,1,0)) %>%
  arrange(date,districtID) %>% #SORT FOR SPACE_TIME
  mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
         timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
         
         Population_density= scale(Population_density),
         Outmigration_Rate=scale(Outmigration_Rate),
         Inmigration_Rate= scale(Inmigration_Rate),
         BI_larvae= scale(BI_larvae),
         Poverty_Rate=scale(Poverty_Rate),
         Monthly_Average_Income_Percapita=scale(Monthly_Average_Income_Percapita),
         Hygienic_Toilet_Access=scale(Hygienic_Toilet_Access),
         Urbanization_Rate=scale(Urbanization_Rate),
  ) 

# check_times <- c1 %>% group_by(district) %>% summarize(N=n())
#check_districts <- c1 %>% group_by(date) %>% summarize(N=n())


formula1 <- 'm_DHF_cases_hold~
                            f(districtID,model = "bym",constr= TRUE, 
                                   graph=MDR.adj)+
                            f(districtID2,lag2_monthly_cum_ppt,model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj) + ##IID random slope
                            lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                        f(t, model="ar1") + #single national-level AR(1)
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


formula1 <- 'm_DHF_cases_hold~ 1 +  Outmigration_Rate +
Inmigration_Rate +   Monthly_Average_Income_Percapita + Urbanization_Rate +
                          lag2_avg_daily_temp+  
                            lag2_avg_max_daily_temp +
                            f(districtID,model = "iid") +
                            f(t, model="ar1") + #single national-level AR(1)
                            f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
# formula1 <- 'm_DHF_cases_hold~
#                             f(districtID,model = "iid")+
#                             urban_dic +
#                             lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
#                             lag2_avg_min_daily_temp*lag2_monthly_cum_ppt +
#                             
#                              lag2_avg_min_daily_temp*urban_dic + lag2_monthly_cum_ppt*urban_dic +
#                              lag2_avg_min_daily_temp*lag2_monthly_cum_ppt*urban_dic +
#                         f(t, model="ar1") + #single national-level AR(1)
#                       f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


form2 <- as.formula (formula1)

#nbinomial or poisson
offset1 <- c1$offset1
mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
             control.compute = list(dic = FALSE, 
                                    waic = TRUE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.predictor = list(compute=TRUE, link=1),
             control.inla = list(strategy='adaptive', # adaptive gaussian
                                 cmin=0),
             control.fixed = list(mean.intercept=0, 
                                  prec.intercept=1, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=N_cores
)    

#coefs <- mod1$summary.fixed['lag2_monthly_cum_ppt','mean']  + mod1$summary.random$districtID2$mean

n.units <- length(unique(c1$districtID))

df.coefs <- mod1$summary.random$districtID2[1:n.units,] #https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html

covars <- c1 %>%
  filter(date=='2012-01-01') %>%
  dplyr::select(districtID2, Urbanization_Rate, Hygienic_Water_Access, Hygienic_Toilet_Access) %>%
  rename(ID=districtID2) %>%
  left_join(.,df.coefs, by='ID')

ggplot(covars, aes(x=mean, y=Hygienic_Toilet_Access)  )+
  geom_point()


ggplot(covars, aes(x=mean, y=Hygienic_Water_Access)  )+
  geom_point()

ggplot(covars, aes(x=mean, y=Urbanization_Rate)  )+
  geom_point()

