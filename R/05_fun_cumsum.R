
cusum_mod <- function(vintage_date, modN){
  

c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop * 100000,
         log_df_rate = log((m_DHF_cases + 1) / pop * 100000),
         log_pop = log(pop / 100000),
         year = lubridate::year(date),
         m_DHF_cases_hold = ifelse(date > vintage_date, NA_real_, m_DHF_cases),
         lag_y = lag(log_df_rate, 1),
         lag2_y = lag(log_df_rate, 2),
         lag3_y = lag(log_df_rate, 3),
         lag3_cum_rain = lag(monthly_cum_ppt,3) ,
         lag3_temp = lag(avg_daily_temp,3) ,
         
         max_allowed_lag = ifelse(any(grepl('lag_y', formula1) | grepl('lag1', formula1)), 1, 3),
         horizon = ifelse(date == (vintage_date %m+% months(2)), 1,
                          ifelse(date == (vintage_date %m+% months(2)), 2,
                                 ifelse(date == (vintage_date %m+% months(3)), 3, 0)
                          )),
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12),
         month=as.factor(month(date)),
         monthN=month(date),
         offset1 = pop/100000,
         #log_offset=log(pop/100000)
  ) %>%
  filter(date<= (vintage_date %m+% months(3) ) & !is.na(lag3_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
  ungroup() %>%
  mutate(
    districtID2 = districtID,
    districtID3 = districtID,
    districtID4 = districtID,
    t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
    
    time_id1= t , 
    time_id2=t,
    time_id3= t,
    time_id4= t,
    
    urban_dic = as.factor(if_else(Urbanization_Rate>=40,1,0))) %>%
  arrange(date,districtID) %>% #SORT FOR SPACE_TIME
  mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
         timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
         Population_density= scale(Population_density)
  )

c1$districtID<- as.numeric(c1$districtID)
# check_times <- c1 %>% group_by(district) %>% summarize(N=n())
#check_districts <- c1 %>% group_by(date) %>% summarize(N=n())

form2 <- as.formula (formula1)

# form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
# hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
#    2)))))    


cum1 <- c1 %>%
  arrange(district, year,date,date) %>%
  group_by(district, year) %>%
  filter(year>=2005) %>%
  mutate(cum_dhf = cumsum(m_DHF_cases+1),
         cumulative_sum = log(cum_dhf),
         splineCumsum = smooth.spline(cumulative_sum)$y,
         derivCumsum = predict(smooth.spline(cumulative_sum), deriv = 1)$y,
         
         derivCumsum=if_else(month==1,is.na(derivCumsum), derivCumsum),
         derivCumsum_hold = ifelse(date > vintage_date, NA_real_, derivCumsum),
         
  ) %>%
  ungroup()%>%
  dplyr::select(date,month, year, m_DHF_cases,pop, monthN,district,t,districtID2,time_id2,lag3_cum_rain, lag3_temp,splineCumsum, cumulative_sum, derivCumsum, derivCumsum_hold,derivCumsum_hold, districtID)


# 
# cum1 %>%
#   filter(district=='AN MINH') %>%
#   ggplot() +
#   geom_line(aes(x=month, y=m_DHF_cases, group=year, color=year)) +
#   theme_classic()
# 
# 
# cum1 %>%
#   filter(district=='AN MINH') %>%
#   ggplot() +
#   geom_line(aes(x=month, y=derivCumsum, group=year, color=year)) +
#   theme_classic()
# 
# cum1 %>%
#   filter(district=='AN MINH') %>%
#   ggplot() +
#   geom_line(aes(x=month, y=splineCumsum, group=year, color=year)) +
#   theme_classic()
# 
# cum1 %>%
#   filter(district=='AN MINH') %>%
#   ggplot() +
#   geom_line(aes(x=date, y=splineCumsum)) +
#   theme_classic()


form1 <- 'derivCumsum_hold ~ 1 +   f(t, model = "ar1", constr=T, hyper=hyper.ar1) + 
                                 f(time_id2, model = "ar1", constr=T,replicate = districtID2, hyper=hyper.ar1) + 
                                f(districtID, model = "iid") + 
                                                    f(monthN, model = "rw1", hyper = hyper2.rw, 
                                                        cyclic = TRUE, scale.model = TRUE, constr = TRUE, 
replicate = districtID2)'

form2 = as.formula((form1))
#nbinomial or poisson

mod1 <- inla(form2, data = cum1,  family = "gaussian",
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
                                  prec.intercept=0.04, # precision 1
                                  mean=0, 
                                  prec=1e-4), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=8
)    

mod.family <- mod1$.args$family

pred <- mod1$summary.linear.predictor %>%
  cbind.data.frame(cum1) %>%
  rename(pred_deriv_log_cum=mean) %>%
  arrange(district, year, date) %>%
  group_by(district, year) %>%
  #now work backwards to get predicted count
  mutate(jan_count= if_else(month==1, m_DHF_cases,NA_real_ ),
         jan_count=max(jan_count, na.rm=T),
         pred_log_cum=cumsum(pred_deriv_log_cum),
         pred_cum=exp(pred_log_cum)*jan_count, #multiply by january N to get appropriate scaling of predictions of counts from the scale-free derivatives
         pred_count = c(NA, diff(pred_cum))
  )

pred %>%
  filter(district=='BEN LUC') %>%
  ggplot() +
  geom_line(aes(x=date, y=derivCumsum)) +
  geom_line(aes(x=date, y=pred_deriv_log_cum), col='red', lty=2) +
  theme_classic()

# p1 <-pred %>%
#   filter(district =='AN PHU') %>%
#   ggplot() +
#   geom_line(aes(x=date, y=m_DHF_cases)) +
#   geom_line(aes(x=date, y=pred_count), col='red') +
#   theme_classic()
# plotly::ggplotly(p1)

# pred %>%
#   filter(district %in% unique(pred$district)[1:6]) %>%
#   ggplot() +
#   geom_line(aes(x=date, y=m_DHF_cases)) +
#   geom_line(aes(x=date, y=pred_count), col='red') +
#   theme_classic()+
#   facet_wrap(~district)


# pred %>%
#   group_by(date) %>%
#   summarize(pred_count=sum(pred_count),
#             m_DHF_cases=sum(m_DHF_cases) ) %>%
#   ggplot() +
#   geom_line(aes(x=date, y=m_DHF_cases)) +
#   geom_line(aes(x=date, y=pred_count), col='red') +
#   theme_classic()

cum1 <- cum1 %>%
  ungroup() %>%
  mutate(forecast= if_else(date>=(vintage_date %m+% months(1)) & date<=(vintage_date %m+% months(3)),1,0  ),
         horizon = if_else(date== (vintage_date %m+% months(1)),1,
                           if_else(date== (vintage_date %m+% months(2)),2,
                                   if_else(date== (vintage_date %m+% months(3)),3,0
                                   )))
         
  ) 

score.list =list ('ds'=cum1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)

scores <- scoring_func_cum(score.list)


c1.out <- cum1 

out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'waic'=mod1$waic,'form'=formula1)
saveRDS(out.list,paste0('./Results/Results_cusum/', 'mod',modN,'_',vintage_date  ,'.rds' )   )
}


#############################
## Scoring function for cumulative sum method
#############################
scoring_func_cum <- function(Y){
  
  in.ds <- Y$ds %>%
    arrange(date,districtID)  #SORTED AS MODEL DS IS SORTED
  
  forecast_ds <- in.ds %>%
    mutate(index=row_number()) %>%
    filter(forecast==1 )
  
  
  forecast.index_horizon1 <- forecast_ds %>%
    filter(forecast==1 & horizon==1) %>%
    pull(index)
  
  forecast.index_horizon2 <- forecast_ds %>%
    filter(forecast==1 & horizon==2) %>%
    pull(index)
  
  forecast.index_horizon3 <- forecast_ds %>%
    filter(forecast==1 & horizon==3) %>%
    pull(index)
  
  forecast.index1 <- sort(c(forecast.index_horizon1,forecast.index_horizon2,forecast.index_horizon3))
  
  test1 <-inla.posterior.sample(1000, Y$mod, seed=0)
  
  #this function extracts the samples for the mean of lambda ('Predictor'), and then generates samples
  #from the predictive distribution using rpois or rnbinom
  
  pred.interval.func <- function(sample.ds){
    pred_deriv_log_cum <- sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))]
    ds2 <- cbind.data.frame(in.ds, pred_deriv_log_cum) %>%
      mutate(year=lubridate::year(date),
             month=lubridate::month(date),
      ) %>%
      arrange(district, year, date) %>%
      group_by(district, year) %>%
      mutate(jan_count= if_else(month==1, m_DHF_cases,NA_real_ ),
             jan_count=max(jan_count, na.rm=T),
             pred_log_cum=cumsum(pred_deriv_log_cum),
             pred_cum=exp(pred_log_cum)*jan_count, #multiply by january N to get appropriate scaling of predictions of counts from the scale-free derivatives
             pred_lambda = c(NA, diff(pred_cum))
      )
    
    lambda2 <- ds2$pred_lambda[forecast.index1]
    lambda2[is.na(lambda2)] <- 0.99898989
    lambda2[lambda2<0] <- 1e-6
    lambda2[lambda2==0] <- 1e-6
    
    pred <- replicate(10, rpois(n=length(lambda2), lambda=lambda2), simplify = 'array')
    
    return(pred)
  }
  
  #Generate samples from the predictive districution for the count
  samps <- sapply(test1,pred.interval.func, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  #convert the count to incidence
  samps.inc <- apply(samps,2, function(x) x/forecast_ds$pop*100000)
  
  #Log(Incidence)
  log.samps.inc <- log(apply(samps,2, function(x) (x+1)/forecast_ds$pop*100000))
  
  log.samps.inc_mean <-apply(log.samps.inc,1,mean)
  
  obs_inc <- (forecast_ds$m_DHF_cases/forecast_ds$pop*100000)
  log_obs_inc <- log((forecast_ds$m_DHF_cases+1)/forecast_ds$pop*100000)
  
  miss.obs <- which(is.na(forecast_ds$m_DHF_cases))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    samps.inc  <- samps.inc[-miss.obs,1,]
  }
  
  #combine the CRPS scores with the 95% posterior predictive distribution (equal tailed)
  out_ds <- forecast_ds %>%
    dplyr::select(date, district,forecast, index,  pop, m_DHF_cases,horizon)%>%
    mutate( pred_mean = apply(samps,1,mean),
            pred_lcl = apply(samps,1,quantile, probs=0.025),
            pred_ucl = apply(samps,1,quantile, probs=0.975))
  
  crps1 <- crps_sample(obs_inc, samps.inc)
  
  crps2 <- crps_sample(log_obs_inc, log.samps.inc) #on the log scale, as recommended by https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1011393#sec008
  
  crps3 <- cbind.data.frame(crps1, crps2,out_ds) 
  
  colnames(log.samps.inc) <- paste0('rep',1:ncol(log.samps.inc))
  
  samps.out <- cbind.data.frame('date'=out_ds$date, 'district'=out_ds$district, 'horizon'=out_ds$horizon, log.samps.inc)
  
  out.list = list('crps3'=crps3, 'log.samps.inc'=samps.out)
  
  return(out.list)
}
