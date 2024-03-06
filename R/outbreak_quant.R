library(dplyr)
library(MASS)
library(lubridate)
library(zoo)
library(ggplot2)
library(exactci)
d2 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors_cleaned.rds') 
  
all.districts <- unique(d2$district) 


#ROBUST REGRESSION
all.rlm <- lapply(all.districts, function(X){
d2a <- d2 %>%
    filter(district==X) %>%
  arrange(district, date) %>%
  mutate(inc = m_DHF_cases/pop*100000,
         log_inc=log((m_DHF_cases+0.5)/pop*100000),
         index=row_number(),
         sin12=sin(2*pi*index/12),
         cos12=cos(2*pi*index/12)
         )


p1 <- ggplot(d2a, aes(x=date, y=log_inc) )+
  geom_line()+
  theme_classic()


  mod1 = rlm(log_inc~sin12 + cos12, data=d2a)

  pred1 <- as.data.frame(exp(predict( mod1, newdata = d2a, interval = "prediction"))) %>%
    mutate(date=d2a$date) %>%
    left_join(d2a, by='date')
  return(pred1)
  })
  
e1 <- bind_rows(all.rlm) %>%
  group_by(district)%>%
  mutate(dist_id =dplyr::cur_group_id()) %>%
  ungroup()

p2 <-e1 %>% filter(dist_id>=2 & dist_id<=7) %>%
   ggplot( aes(x=date, y=inc) )+
    geom_line()+
    geom_line(aes(x=date, y=fit), col='red')+
   # geom_ribbon( aes(x=date, ymin=lwr, ymax=upr), fill='blue', alpha=0.2)+
    theme_classic()+
    facet_wrap(~district, nrow=3, ncol=2)
p2

#############################################
##USING THE VIETNAMESE MOH DEFINITION
#############################################
#NOE TO FIX--THIS ISN"T CURRENTLY CORRECT--most.recent is taking 5 most recent, but it should be 5 most recent not including current time

dates.test <- sort(unique(d2$date))[-c(1:60)] #exclude first 5 years

for(i in 1:length(dates.test)){
  X=dates.test[i]
  month.test <- month(X)
  
  most.recent <- ds1.in %>%
    filter(date < X & monthN == month.test ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, not including current month
    arrange(date) %>%
    mutate(order=row_number(),
           rev_order= max(order)- order+ 1) %>%
    filter(rev_order<=5)
  
  #mean with SD
  ds1.test_mean_sd <- most.recent %>%
    summarize(n_times=n(),
              ma5=mean(N_cases,na.rm=T), #5 most recent NON EPIDEMIC years
              sd5=sd(N_cases, na.rm=T)
    ) %>%
    mutate(date=X)  
  
  #quantile
  ds1.test_quant <- ds1.in %>%  #note uses whole data, not just most recent 5
    filter(date < X & monthN == month.test ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, excluding past epidemics
    summarize(n_times=n(),
              mquant=quantile(N_cases, probs=0.95),
    ) %>%
    ungroup() %>%
    mutate(date=X)
  #poisson
  #pois.pred.interval= quantile(rpois(100000, lambda=mean.x), probs=0.975)
  
  #poisson prediction interval
  mod1.coef <-summary(glm(N_cases ~1, family='poisson', data=most.recent))$coefficients['(Intercept)',c('Estimate','Std. Error')]
  
  pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] +rnorm(1000,mean=0, sd=mod1.coef['Std. Error']))), probs=0.975)
  
  #for the most recent date, determine if an epidemic has occurred
  ds1.in <- bind_rows(ds1.in, ds1[ds1$date==X,]) #add on latest observation 
  
  ds1.in <- ds1.in %>%
    mutate( threshold= if_else(date==X,ds1.test_mean_sd$ma5 + 2*ds1.test_mean_sd$sd5 ,threshold),
            epidemic_flag = if_else(date==X,1*(N_cases>threshold),epidemic_flag),
            
            threshold_poisson= if_else(date==X,pois.ucl ,threshold_poisson),
            epidemic_flag_poisson = if_else(date==X,1*(N_cases>threshold_poisson),epidemic_flag_poisson),
            
            threshold_quant= if_else(date==X,ds1.test_quant$mquant ,threshold_quant),
            epidemic_flag_quant = if_else(date==X,1*(N_cases>threshold_quant),epidemic_flag_quant),
            
    )
  
}

ds2_pois <- ds1.in %>%
  filter( epidemic_flag_poisson!=-999)
ggplot(ds2_pois, aes(x=date, y=N_cases)) +
  geom_line(col='gray') +
  theme_classic() +
  geom_line(aes(x=date, y=threshold_poisson))+
  geom_line(aes(x=date, y=threshold), col='red')+
  geom_line(aes(x=date, y=threshold_quant), col='blue')

mean(ds2_pois$epidemic_flag_pois)
mean(ds2_pois$epidemic_flag_quant)
mean(ds2_pois$epidemic_flag)