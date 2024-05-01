library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(purrr)
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


c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop *10000,
         log_df_lag2 = lag(log((m_DHF_cases+0.5) / pop *10000),n=2),
         log_df_lag3 = lag(log((m_DHF_cases+0.5) / pop *10000),n=3),
         log_df_lag4 = lag(log((m_DHF_cases+0.5) / pop *10000),n=4),
         log_df_lag5 = lag(log((m_DHF_cases+0.5) / pop *10000),n=5),
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12)
         #log_offset=log(pop/100000)
  )
c1.train <- c1 %>%
  filter(date<='2021-08-01' & district=='VINH HUNG') %>%
  mutate( m_DHF_cases_train = ifelse(date<='2021-06-01',m_DHF_cases, NA_real_  ) ) %>%
  dplyr::select(date, district, m_DHF_cases,pop,Dengue_fever_rates)


plot(c1.train$date,c1.train$Dengue_fever_rates)

write.csv(c1.train,'./Data/test1.csv')


c1.train.dist <- c1 %>%
  filter(date<='2021-08-01' ) %>%
  mutate( m_DHF_cases_train = ifelse(date<='2021-06-01',m_DHF_cases, NA_real_  ) ) %>%
  group_by(district) %>%
  mutate(districtID =cur_group_id()     )%>%
  dplyr::select(date, district, m_DHF_cases,pop,Dengue_fever_rates)
write.csv(c1.train.dist,'./Data/test2.csv')


all.lags <- c1 %>%
  dplyr::select(district,date,log_df_lag2,log_df_lag3,log_df_lag4,log_df_lag5
                ) %>%
  reshape2::melt(., id.vars=c('district','date')) %>%
  reshape2::dcast(., date ~ district+variable) %>%
  filter(complete.cases(.)) %>%
  mutate_at( vars(-starts_with("date")), ~ if(is.numeric(.)) scale(.) else .) %>% #scale all variables except date
  clean_names()

district.select = 'VINH HUNG'

date.test <- '2015-06-01'
  
c2 <- c1 %>%
  filter(date<='2021-08-01' & district==district.select) %>%
  left_join(all.lags, by='date') %>%
  mutate(offset1= pop/10000,
         m_DHF_cases_hold= ifelse( date>= (date.test), NA_real_,
                                   m_DHF_cases)
         )
 # dplyr::select(-contains(district.select)) #filters out lags from the select district--fix this to work with tidy names


all.vars <- paste(names(all.lags)[-1], collapse="+")
form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " + sin12 +cos12 + f(t, model='ar1')"))

offset1 <- c2$offset1
ptm <- proc.time()

mod1 <- inla(form2, data = c2,  family = "poisson",E=offset1,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
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
             num.threads=7
)    

proc.time() - ptm

# pc.mat <- as.matrix(all.lags[,-1])
# pc1 <- prcomp(pc.mat)
# plot(pc1$sdev)
# # for a single district, use lags of all other districts
# matplot(pc1$rotation[,1:10], type='l')
