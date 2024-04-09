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


c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop *100000
         #log_offset=log(pop/100000)
  ) %>%
  dplyr::select(date, district, m_DHF_cases,pop,Dengue_fever_rates)

c1.train <- c1 %>%
  filter(date<='2021-08-01' & district=='VINH HUNG') %>%
  mutate( m_DHF_cases_train = if_else(date<='2021-06-01',m_DHF_cases, NA_real_  ) )

plot(c1.train$date,c1.train$Dengue_fever_rates)

write.csv(c1.train,'./Data/test1.csv')


c1.train.dist <- c1 %>%
  filter(date<='2021-08-01' ) %>%
  mutate( m_DHF_cases_train = if_else(date<='2021-06-01',m_DHF_cases, NA_real_  ) ) %>%
  group_by(district) %>%
  mutate(districtID =cur_group_id()     )
