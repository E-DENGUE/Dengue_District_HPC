library(dplyr)
library(MASS)
library(lubridate)
library(zoo)
library(ggplot2)
library(exactci)
library(RcppRoll)
library(patchwork)
d2 <- readRDS('./Model/Data/Full_data_set_with_covariates_and_lags.rds')

all.fcodes <- unique(d2$fcode) 


#ROBUST REGRESSION
all.rlm <- lapply(all.fcodes, function(X){
  d2a <- d2 %>%
    filter(fcode==X) %>%
    arrange(fcode, date) %>%
    mutate(inc = obs_dengue_cases/pop_total*100000,
           log_inc=log((obs_dengue_cases+0.5)/pop_total*100000),
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
  group_by(fcode)%>%
  mutate(dist_id =dplyr::cur_group_id()) %>%
  ungroup()

p2 <-e1 %>% filter(dist_id>=2 & dist_id<=7) %>%
  ggplot( aes(x=date, y=inc) )+
  geom_line()+
  geom_line(aes(x=date, y=fit), col='red')+
  # geom_ribbon( aes(x=date, ymin=lwr, ymax=upr), fill='blue', alpha=0.2)+
  theme_classic()+
  facet_wrap(~fcode, nrow=3, ncol=2)
p2

#############################################
##USING THE VIETNAMESE MOH DEFINITION
#############################################

dates.test <- sort(unique(d2$date))[-c(1:60)]
ds1.in <- d2 %>%
  dplyr::select(date, obs_dengue_cases ,pop_total, fcode) %>%
  mutate(monthN=lubridate::month(date),
         epidemic_flag=-9999,
         epidemic_flag_poisson=-9999,
         epidemic_flag_poisson2=-9999,
         epidemic_flag_nb=-9999,
         epidemic_flag_quant=-9999,
         
         threshold_poisson=9999,
         threshold_poisson2=9999,
         threshold_nb=9999,
         threshold=9999,
         threshold_quant=9999
  )

ds1.in.dist <- split(ds1.in, ds1.in$fcode) 
ds1.in.dist.init <- list()

for(j in 1:length(unique(ds1.in$fcode))){
  ds1.in.dist.init[[j]] <- ds1.in.dist[[j]] %>% filter(date<dates.test[1])
  dist.select <- unique( ds1.in.dist.init[[j]]$fcode)
  print(j)
  for(i in 1:length(dates.test)){
    X=dates.test[i]
    month.test <- month(X)
    
    most.recent <- ds1.in.dist.init[[j]] %>%
      filter(date < X & fcode==dist.select & monthN == month.test & epidemic_flag<=0 ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, not including current month
      arrange(date) %>%
      mutate(order=row_number(),
             rev_order= max(order)- order+ 1) %>%
      filter(rev_order<=5)
    
    #mean with SD
    ds1.test_mean_sd <- most.recent %>%
      summarize(n_times=n(),
                ma5=mean(obs_dengue_cases,na.rm=T), #5 most recent NON EPIDEMIC years
                sd5=sd(obs_dengue_cases, na.rm=T)
      ) %>%
      mutate(date=X)  
    
    #quantile
    ds1.test_quant <- ds1.in.dist.init[[j]] %>%  #note uses whole data, not just most recent 5
      filter(date < X & fcode==dist.select & monthN == month.test  & epidemic_flag<=0) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, excluding past epidemics
      summarize(n_times=n(),
                mquant=quantile(obs_dengue_cases, probs=0.95),
      ) %>%
      ungroup() %>%
      mutate(date=X)
    #poisson
    #pois.pred.interval= quantile(rpois(100000, lambda=mean.x), probs=0.975)
    
    #poisson prediction interval
    if(sum(most.recent$obs_dengue_cases>0)){
      mod1.coef <-summary(glm(obs_dengue_cases ~1, family='poisson', data=most.recent))$coefficients['(Intercept)',c('Estimate','Std. Error')]
      pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] +rnorm(1000,mean=0, sd=mod1.coef['Std. Error']))), probs=0.975)
    }else{
      pois.ucl <- 5 #if no cases observed, just set arbitrary threshold of 5
    }
    
    #Alternative Poisson prediction interval (as in surveillance package)
    pois.ucl2 <- qpois(0.95,lambda=mean(most.recent$obs_dengue_cases))
    
    #NegBin prediction interval, as in algo.bayes in surveillance package
    nb.ucl <- qnbinom(0.95, size=sum(most.recent$obs_dengue_cases)+0.5 , length(most.recent$obs_dengue_cases)/(length(most.recent$obs_dengue_cases) + 1) ) 
    
    #for the most recent date, determine if an epidemic has occurred
    add.ds <- ds1.in.dist[[j]] %>% filter(date==X)
    ds1.in.dist.init[[j]] <- bind_rows(ds1.in.dist.init[[j]], add.ds) #add on latest observation 
    
    ds1.in.dist.init[[j]] <- ds1.in.dist.init[[j]] %>%
      mutate( threshold= if_else(date==X,ds1.test_mean_sd$ma5 + 2*ds1.test_mean_sd$sd5 ,threshold),
              epidemic_flag = if_else(date==X,1*(obs_dengue_cases>threshold),epidemic_flag),
              
              threshold_poisson= if_else(date==X,pois.ucl ,threshold_poisson),
              epidemic_flag_poisson = if_else(date==X,1*(obs_dengue_cases>threshold_poisson),epidemic_flag_poisson),
              
              threshold_poisson2= if_else(date==X,pois.ucl2 ,threshold_poisson2),
              epidemic_flag_poisson2 = if_else(date==X,1*(obs_dengue_cases>threshold_poisson2),epidemic_flag_poisson2),
              
              threshold_nb= if_else(date==X,nb.ucl,threshold_nb),
              epidemic_flag_nb = if_else(date==X,1*(obs_dengue_cases>threshold_nb),epidemic_flag_nb),
              
              threshold_quant= if_else(date==X,ds1.test_quant$mquant ,threshold_quant),
              epidemic_flag_quant = if_else(date==X,1*(obs_dengue_cases>threshold_quant),epidemic_flag_quant),
              
      )
    
  }
}

out.ds <- bind_rows(ds1.in.dist.init)%>%
  mutate(obs_inc=obs_dengue_cases/pop_total*100000)
saveRDS(out.ds,'./Model/Data/thres_ds1_2025.rds')

plot.fcodes10 <- unique(out.ds$fcode)[1:10]

ds2_pois10 <- out.ds %>%
  filter( threshold_quant !=9999 & fcode %in% plot.fcodes10) %>%
  mutate(epidemic_flag_fixed = if_else(obs_inc>50,1,0)) # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25


ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  facet_wrap(~fcode, scales='fixed') +
  geom_hline(yintercept=50, col='gray')+ # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_fixed)) 


ggplot(ds2_pois10, aes(x=date, y=obs_dengue_cases)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_quant)) +
  geom_line(aes(x=date, y=threshold_quant/pop_total*100000), col='gray', lty=2, alpha=0.5)+
  facet_wrap(~fcode, scales='fixed')

ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag)) +
  geom_line(aes(x=date, y=threshold/pop_total*100000), col='gray', lty=2, alpha=0.5)+
  facet_wrap(~fcode, scales='fixed')

ggplot(ds2_pois10, aes(x=date, y=obs_inc)) +
  geom_line(col='black') +
  theme_classic() +
  geom_point(aes(x=date, y=obs_inc, color=epidemic_flag_poisson)) +
  geom_line(aes(x=date, y=threshold_poisson/pop_total*100000) ,lty=2, alpha=0.5,col='gray')+
  facet_wrap(~fcode, scales='fixed')


plot.fcodes <- unique(out.ds$fcode)

ds2_pois <- out.ds %>%
  filter( threshold_quant !=9999 ) %>%
  mutate(epidemic_flag_fixed = if_else(obs_inc>50,1,0)) # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25

mean(ds2_pois$epidemic_flag_poisson)
mean(ds2_pois$epidemic_flag_poisson2)
mean(ds2_pois$epidemic_flag_nb)

mean(ds2_pois$epidemic_flag_quant)
mean(ds2_pois$epidemic_flag)
mean(ds2_pois$epidemic_flag_fixed, na.rm=T)

## quantify performance of the different cutoffs. We want to have high values for:
### what proportion of cases in each year occur after the epidemic is observed?
### what is the incidence of cases that occur each year after the epidemic is declared
##This creates a 4 month moving window for each fcode to see if an alarm has been triggered in last 4 months (0=no alarm)
e1 <- ds2_pois %>%
  arrange(fcode, date) %>%
  mutate(year=lubridate::year(date)) %>%
  group_by(fcode) %>%
  mutate(alarmN= RcppRoll::roll_sum(epidemic_flag, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_pois=RcppRoll::roll_sum(epidemic_flag_poisson, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_quant=RcppRoll::roll_sum(epidemic_flag_quant, n=4,align = "right", fill = NA,partial = FALSE),
         alarmN_fixed=RcppRoll::roll_sum(epidemic_flag_fixed, n=4,align = "right", fill = NA,partial = FALSE))%>%
  ungroup()%>%
  #Count how many cases occur during the alarm period
  mutate(N_epidemic_sd = ifelse(alarmN>0,obs_dengue_cases,NA_real_) ,
         N_epidemic_pois = ifelse(alarmN_pois>0,obs_dengue_cases,NA_real_) ,
         N_epidemic_quant = ifelse(alarmN_quant>0,obs_dengue_cases,NA_real_) ,
         N_epidemic_fixed = ifelse(alarmN_fixed>0,obs_dengue_cases,NA_real_) ,
         inc=obs_dengue_cases/pop_total*100000
  ) %>%
  filter(!is.na(alarmN)) 

e1_summary <- e1%>%
  group_by(fcode, year) %>% 
  summarize(N_epidemic_sd=sum(N_epidemic_sd, na.rm=T),
            N_epidemic_pois=sum(N_epidemic_pois, na.rm=T),
            N_epidemic_quant=sum(N_epidemic_quant, na.rm=T),
            N_epidemic_fixed=sum(N_epidemic_fixed, na.rm=T),
            N_cases=sum(obs_dengue_cases, na.rm=T),
            pop_total=mean(pop_total),
            
            prop_epidemic_2sd = N_epidemic_sd/N_cases,
            prop_epidemic_quant = N_epidemic_quant/N_cases,
            prop_epidemic_pois = N_epidemic_pois/N_cases,
            prop_epidemic_fixed = N_epidemic_fixed/N_cases,
            
            inc_epidemic_2sd = N_epidemic_sd/pop_total*100000,
            inc_epidemic_quant = N_epidemic_quant/pop_total*100000,
            inc_epidemic_pois = N_epidemic_pois/pop_total*100000,
            inc_epidemic_fixed = N_epidemic_fixed/pop_total*100000,
  )
saveRDS(e1, './Model/Data/observed_alarms_2025.rds')
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

################
#SHINY APP TO EXPLORE FIXED THRESHOLD

library(shiny)
threshold_app <- function(){
  
  shinyApp( ui = fluidPage(    
    sliderInput("threshold", "Threshold (Incidence per month:",
                min=10, max=120, value=40),
    plotOutput("threshPlot")  
  ),
  
  server = function(input, output) {
    output$threshPlot = renderPlot({
      ds.plot <- out.ds %>%
        filter( threshold_quant !=9999 ) %>%
        mutate(epidemic_flag_fixed = ifelse(obs_inc>input$threshold,1,0)) %>% # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25
        arrange(fcode, date) %>%
        mutate(year=year(date)) %>%
        group_by(fcode) %>%
        mutate( alarmN_fixed=RcppRoll::roll_sum(epidemic_flag_fixed, n=4,align = "right", fill = NA,partial = FALSE))%>%
        ungroup()%>%
        #Count how many cases occur during the alarm period
        mutate( inc=obs_dengue_cases/pop_total*100000,
                N_epidemic_fixed = ifelse(alarmN_fixed>0,obs_dengue_cases,NA_real_) 
        ) %>%
        group_by(fcode, year) %>% 
        summarize(
          N_epidemic_fixed=sum(N_epidemic_fixed, na.rm=T),
          N_cases=sum(obs_dengue_cases, na.rm=T),
          pop_total=mean(pop_total),
          
          prop_epidemic_fixed = N_epidemic_fixed/N_cases,
          
          inc_epidemic_fixed = N_epidemic_fixed/pop_total*100000,
        ) %>%
        ungroup()
      ##What we want is stuff in upper quadrant: large proportion of cases occur after epidemic is declared and incidence is high after epidemic is declared.
      p4<- ggplot(ds.plot, aes(x=inc_epidemic_fixed, y=prop_epidemic_fixed))+
        geom_point(alpha=0.2)+
        ylim(0,1)+
        ggtitle('Fixed threshold')+
        theme_classic()
      p4  
    },width = "auto", height = "auto")
  }
  )
}

threshold_app()

