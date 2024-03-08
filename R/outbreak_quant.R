library(dplyr)
library(MASS)
library(lubridate)
library(zoo)
library(ggplot2)
library(exactci)
library(RcppRoll)
library(patchwork)
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

dates.test <- sort(unique(d2$date))[-c(1:60)]
ds1.in <- d2 %>%
  dplyr::select(date, m_DHF_cases ,pop, district) %>%
  mutate(monthN=lubridate::month(date),
         epidemic_flag=-9999,
         epidemic_flag_poisson=-9999,
         epidemic_flag_quant=-9999,
         
                  threshold_poisson=9999,
         threshold=9999,
         threshold_quant=9999
         )

ds1.in.dist <- split(ds1.in, ds1.in$district) 
ds1.in.dist.init <- list()

for(j in 1:length(unique(ds1.in$district))){
  ds1.in.dist.init[[j]] <- ds1.in.dist[[j]] %>% filter(date<dates.test[1])
  dist.select <- unique( ds1.in.dist.init[[j]]$district)
  print(j)
  for(i in 1:length(dates.test)){
  X=dates.test[i]
  month.test <- month(X)
  
  most.recent <- ds1.in.dist.init[[j]] %>%
    filter(date < X & district==dist.select & monthN == month.test & epidemic_flag<=0 ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, not including current month
    arrange(date) %>%
    mutate(order=row_number(),
           rev_order= max(order)- order+ 1) %>%
    filter(rev_order<=5)
  
  #mean with SD
  ds1.test_mean_sd <- most.recent %>%
    summarize(n_times=n(),
              ma5=mean(m_DHF_cases,na.rm=T), #5 most recent NON EPIDEMIC years
              sd5=sd(m_DHF_cases, na.rm=T)
    ) %>%
    mutate(date=X)  
  
  #quantile
  ds1.test_quant <- ds1.in.dist.init[[j]] %>%  #note uses whole data, not just most recent 5
    filter(date < X & district==dist.select & monthN == month.test  & epidemic_flag<=0) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, excluding past epidemics
    summarize(n_times=n(),
              mquant=quantile(m_DHF_cases, probs=0.95),
    ) %>%
    ungroup() %>%
    mutate(date=X)
  #poisson
  #pois.pred.interval= quantile(rpois(100000, lambda=mean.x), probs=0.975)
  
  #poisson prediction interval
  if(sum(most.recent$m_DHF_cases>0)){
    mod1.coef <-summary(glm(m_DHF_cases ~1, family='poisson', data=most.recent))$coefficients['(Intercept)',c('Estimate','Std. Error')]
    pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] +rnorm(1000,mean=0, sd=mod1.coef['Std. Error']))), probs=0.975)
  }else{
    pois.ucl <- 5 #if no cases observed, just set arbitrary threshold of 5
  }
    

  #for the most recent date, determine if an epidemic has occurred
  add.ds <- ds1.in.dist[[j]] %>% filter(date==X)
  ds1.in.dist.init[[j]] <- bind_rows(ds1.in.dist.init[[j]], add.ds) #add on latest observation 
  
  ds1.in.dist.init[[j]] <- ds1.in.dist.init[[j]] %>%
    mutate( threshold= if_else(date==X,ds1.test_mean_sd$ma5 + 2*ds1.test_mean_sd$sd5 ,threshold),
            epidemic_flag = if_else(date==X,1*(m_DHF_cases>threshold),epidemic_flag),
            
            threshold_poisson= if_else(date==X,pois.ucl ,threshold_poisson),
            epidemic_flag_poisson = if_else(date==X,1*(m_DHF_cases>threshold_poisson),epidemic_flag_poisson),
            
            threshold_quant= if_else(date==X,ds1.test_quant$mquant ,threshold_quant),
            epidemic_flag_quant = if_else(date==X,1*(m_DHF_cases>threshold_quant),epidemic_flag_quant),
            
    )
  
  }
}

out.ds <- bind_rows(ds1.in.dist.init)%>%
  mutate(obs_inc=m_DHF_cases/pop*100000)

plot.districts10 <- unique(out.ds$district)[1:10]

ds2_pois10 <- out.ds %>%
  filter( threshold_quant !=9999 & district %in% plot.districts10) %>%
  mutate(epidemic_flag_fixed = if_else(obs_inc>43.75,1,0)) # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25


ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  facet_wrap(~district, scales='fixed') +
  geom_hline(yintercept=43.75, col='gray')+ # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_fixed)) 
  

ggplot(ds2_pois10, aes(x=date, y=m_DHF_cases)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_quant)) +
  geom_line(aes(x=date, y=threshold_quant/pop*100000), col='gray', lty=2, alpha=0.5)+
  facet_wrap(~district, scales='fixed')

ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag)) +
  geom_line(aes(x=date, y=threshold/pop*100000), col='gray', lty=2, alpha=0.5)+
  facet_wrap(~district, scales='fixed')

ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_poisson)) +
  geom_line(aes(x=date, y=threshold_poisson/pop*100000) ,lty=2, alpha=0.5,col='gray')+
  facet_wrap(~district, scales='fixed')


plot.districts <- unique(out.ds$district)

ds2_pois <- out.ds %>%
  filter( threshold_quant !=9999 ) %>%
  mutate(epidemic_flag_fixed = if_else(obs_inc>43.75,1,0)) # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25

mean(ds2_pois$epidemic_flag_poisson)
mean(ds2_pois$epidemic_flag_quant)
mean(ds2_pois$epidemic_flag)
mean(ds2_pois$epidemic_flag_fixed, na.rm=T)

## quantify performance of the different cutoffs. We want to have high values for:
### what proportion of cases in each year occur after the epidemic is observed?
### what is the incidence of cases that occur each year after the epidemic is declared
##This creates a 4 month moving window for each district to see if an alarm has been triggered in last 4 months (0=no alarm)
e1 <- ds2_pois %>%
  arrange(district, date) %>%
  mutate(year=year(date)) %>%
  group_by(district) %>%
  mutate(alarmN= RcppRoll::roll_sum(epidemic_flag, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_pois=RcppRoll::roll_sum(epidemic_flag_poisson, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_quant=RcppRoll::roll_sum(epidemic_flag_quant, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_fixed=RcppRoll::roll_sum(epidemic_flag_fixed), n=4,align = "right", fill = NA,partial = FALSE)%>%
  ungroup()%>%
  mutate(N_epidemic_sd = if_else(alarmN>0,m_DHF_cases,NA_real_) ,
         N_epidemic_pois = if_else(alarmN_pois>0,m_DHF_cases,NA_real_) ,
         N_epidemic_quant = if_else(alarmN_quant>0,m_DHF_cases,NA_real_) ,
         N_epidemic_fixed = if_else(alarmN_fixed>0,m_DHF_cases,NA_real_) 
        ) %>%
  filter(!is.na(alarmN)) 

 e1_summary <- e1%>%
   group_by(district, year) %>% 
  summarize(N_epidemic_sd=sum(N_epidemic_sd, na.rm=T),
            N_epidemic_pois=sum(N_epidemic_pois, na.rm=T),
            N_epidemic_quant=sum(N_epidemic_quant, na.rm=T),
            N_epidemic_fixed=sum(N_epidemic_fixed, na.rm=T),
            N_cases=sum(m_DHF_cases, na.rm=T),
            pop=mean(pop),
            
            prop_epidemic_2sd = N_epidemic_sd/N_cases,
            prop_epidemic_quant = N_epidemic_quant/N_cases,
            prop_epidemic_pois = N_epidemic_pois/N_cases,
            prop_epidemic_fixed = N_epidemic_fixed/N_cases,
            
            inc_epidemic_2sd = N_epidemic_sd/pop*100000,
            inc_epidemic_quant = N_epidemic_quant/pop*100000,
            inc_epidemic_pois = N_epidemic_pois/pop*100000,
            inc_epidemic_fixed = N_epidemic_fixed/pop*100000,
            )
saveRDS(e1, './Data/observed_alarms.rds')
##What we want is stuff in upper quadrant: large proportion of cases occur after epidemic is declared and incidence is high after epidemic is declared.
p1<-ggplot(e1_summary, aes(x=inc_epidemic_2sd, y=prop_epidemic_2sd))+
  geom_point(alpha=0.2)+
  ylim(0,1)+
  ggtitle('Mean+2SD')+
  theme_classic()

p2<- ggplot(e1_summary, aes(x=inc_epidemic_quant, y=prop_epidemic_quant))+
  geom_point(alpha=0.2)+
  ylim(0,1)+
  ggtitle('Quantile (95%tile)')+
  theme_classic()

p3 <- ggplot(e1_summary, aes(x=inc_epidemic_pois, y=prop_epidemic_pois))+
  geom_point(alpha=0.2)+
  ylim(0,1)+
  ggtitle('Poisson prediction interval')+
  theme_classic()


p4<- ggplot(e1_summary, aes(x=inc_epidemic_fixed, y=prop_epidemic_fixed))+
  geom_point(alpha=0.2)+
  ylim(0,1)+
ggtitle('Fixed threshold')+
  theme_classic()

(p1+p2)/(p3+p4)
  
