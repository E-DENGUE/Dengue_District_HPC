library(roll)
library(dplyr)
# set.seed(123)
# ds1 <- tibble(Y =rnorm(120,0,1)) %>%
#   mutate(t=row_number(),
#          cumsum_cases_12m =  roll_sum(Y,12, min_obs=1),
#          lag12_cumsum_cases_12m = lag(cumsum_cases_12m,12),
#          cumsum_cases_24m =  roll_sum(Y,24, min_obs=1),
#          cumsum_cases_12_24m = cumsum_cases_24m - cumsum_cases_12m)
# 
# ggplot(data=ds1) +
#   geom_line(aes(x=t, y=cumsum_cases_12m)) + 
#   geom_line(aes(x=t, y=cumsum_cases_24m) )+
#   geom_line(aes(x=t, y=cumsum_cases_12_24m))+
#   geom_line(aes(x=t, y=lag12_cumsum_cases_12m), color='red')

window_func <- function(window.width=24, lagN=24){

  #restrict sizes  
#lagN = if_else(lagN>=36,36,lagN)
#window.width = if_else(lagN>=36,36,lagN)

d2 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  rename(District_province = prov_dis) %>%
  arrange(district, date) %>%
  group_by(district) %>%
  mutate(   cumsum_cases =  roll_sum(m_DHF_cases,window.width, min_obs=1), #partial backward moving sum
            cumsum_pop =  roll_sum(pop,window.width, min_obs=1), 
            cum_inc = (cumsum_cases+1)/cumsum_pop,
            cumsum_inc_lag = dplyr::lag(cum_inc,lagN)
            
  ) %>%
  ungroup() %>%
  mutate(log_cumsum_inc_lag=scale(log(cumsum_inc_lag))[,1]
  ) #%>%
 # filter(date<='2016-01-01' & date>='2008-01-01') 


mod1 <- glm( m_DHF_cases ~   log_cumsum_inc_lag + lag2_avg_daily_temp +district , offset=log(pop/100000) ,family='poisson', data=d2)
#summary(mod1)
AIC(mod1)

}

cumN = c(6,12,24,36)
lagN = c(6,12,18,24,30,36)

ds.test <- expand.grid(cumN, lagN) %>% 
  rename(cumN=Var1, lagN=Var2) %>%
  filter(cumN + lagN >= 48)
ds.test.spl <- split(ds.test, 1:nrow(ds.test))

 lapply(ds.test.spl, function(X)  window_func(window.width=X$cumN, lagN=X$lagN) )

 #Best is a 24 month cumulative sum, lagged by 24 months 
 ds.test[3,]
 