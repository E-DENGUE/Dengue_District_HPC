source('./R/99_load.R')
j=1
k=1
i=1
mods <- c('PC_lags_weather','PC_lags','PC_weather')

vintage_date = date.test2[j] 
district.select=all.districts[k]
modN=i



c1a <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop *100000,
         log_df_lag2 = lag(log((m_DHF_cases+0.5) / pop *100000),n=2),
         log_df_lag3 = lag(log((m_DHF_cases+0.5) / pop *100000),n=3),
         log_df_lag4 = lag(log((m_DHF_cases+0.5) / pop *100000),n=4),
         log_df_lag5 = lag(log((m_DHF_cases+0.5) / pop *100000),n=5),
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12),
         
  ) %>%
  filter(date<= (vintage_date[1] %m+% months(3) )) %>%  #only keep vintage date and 2 month ahead of that
  ungroup() %>%
  mutate(t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
         time_id1= t ) 

all.lags <- c1a %>%
  dplyr::select(district,date,log_df_lag3,log_df_lag4,log_df_lag5
  ) %>%
  reshape2::melt(., id.vars=c('district','date')) %>%
  reshape2::dcast(., date  ~ district+variable) %>% 
  filter(complete.cases(.)) %>%
  mutate_at( vars(-starts_with("date")), ~ if(is.numeric(.)) scale(.) else .) %>% #scale all variables except date
  clean_names()

# form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
# hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
#    2)))))    

# Assuming all.lag is a list where each element is a data frame or matrix


#nbinomial or poisson

c1b <- c1a %>%
  filter( district==district.select) %>%
  left_join(all.lags, by='date') %>%
  mutate(offset1= pop/100000,
         m_DHF_cases_hold= ifelse( date> (vintage_date), NA_real_,
                                   m_DHF_cases)
  )
# dplyr::select(-contains(district.select)) #filters out lags from the select district--fix this to work with tidy names

##Y-AWARE PCA
df2 <- c1b %>% filter( !is.na(an_minh_log_df_lag4 ))
x <- df2[,names(all.lags)]
Y <- df2$m_DHF_cases_hold
y.aware.scale<- apply(x[,-1], 2, function(x1){
  x1 <- as.vector(x1)
  log.y.pre.scale<- scale(log(Y+0.5))
  log.y.pre.scale<- as.vector(log.y.pre.scale)
  reg<-lm(log.y.pre.scale~x1)
  slope<- reg$coefficients[2]
  x.scale<-x1*slope - mean(x1*slope)
  return(x.scale)
})
pca1<- prcomp(y.aware.scale, center = FALSE,scale. = FALSE)
# plot(pca1$sdev)
n.pcs.keep<-10
pcs<-pca1$x
pcs<- as.data.frame(apply(pcs,2, scale)) #SCALE THE PCS prior to regression!
names(pcs) <- paste0('PC', 1:ncol(pcs))
pc.df <- cbind.data.frame('date'=df2$date, pcs[,1:n.pcs.keep])

c1 <- c1b %>%
  left_join(pc.df, by='date') 


all.vars <- paste(names(pc.df)[-1], collapse="+")

mod.select = mods[as.numeric(modN)]

if(mod.select == 'PC_lags_weather'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " +lag2_monthly_cum_ppt + lag2_avg_daily_temp + sin12 +cos12 + f(t, model='ar1')"))
}else   if(mod.select == 'PC_lags'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " + sin12 +cos12 + f(t, model='ar1')"))
}else   if(mod.select == 'PC_weather'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", "lag2_monthly_cum_ppt + lag2_avg_daily_temp + sin12 +cos12 + f(t, model='ar1')"))
  
}



form2 <- as.formula(paste0("m_DHF_cases_hold ~",  " sin12 +cos12 + f(t, model='ar1')"))


offset1 <- c1$offset1
#ptm <- proc.time()

mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
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
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=7
)    


mod.family <- mod1$.args$family


c1 <- c1 %>%
  ungroup() %>%
  mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
         horizon = if_else(date== (vintage_date %m+% months(1)),1,
                           if_else(date== (vintage_date %m+% months(2)),2,
                                   if_else(date== (vintage_date %m+% months(3)),3, 0
                                   )
                           )),
         max_allowed_lag = 3
  )%>%
  filter(horizon <= max_allowed_lag) #get rid of lag2 if lag1 is included as covariate
#View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))


score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)

scoring_func <- function(Y){
  
  in.ds <- Y$ds %>%
    arrange(date,districtID)  #SORTED AS MODEL DS IS SORTED
  
  forecast_ds <- in.ds %>%
    mutate(index=row_number()) #%>%
   # filter(forecast==1 )
  
  
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
  
  pred.interval.func <- function(sample.ds, dist=c('nb','poisson')){
    lambda1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])*in.ds$pop/100000 #"Predictor"=lambda/E 
    lambda2 <- lambda1
    if(dist=='nb'){
      nb.size1 = sample.ds$hyperpar['size for the nbinomial observations (1/overdispersion)']
      pred <- replicate(10, rnbinom(n=length(lambda2), mu=lambda2, size=nb.size1), simplify = 'array')
    }else{
      pred <- replicate(10, rpois(n=length(lambda2), lambda=lambda2), simplify = 'array')
    }
    return(pred)
  }
  
  #Generate samples from the predictive districution for the count
  samps <- sapply(test1,pred.interval.func,dist=Y$mod.family, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  #convert the count to incidence
  samps.inc <- apply(samps,2, function(x) x/offset1)
  
  #Log(Incidence)
  log.samps.inc <- log(apply(samps,2, function(x) (x+1)/offset1))
  
  log.samps.inc_mean <-apply(log.samps.inc,1,mean)
  
  obs_inc <- (c1$m_DHF_cases/offset1)
  log_obs_inc <- log((c1$m_DHF_cases+1)/offset1)
  
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


scores <- scoring_func(score.list)

#plot(c1$date, mod1$summary.fitted.values$mean)
# plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))

c1.out <- c1 %>%
  dplyr::select(date, district, Dengue_fever_rates, m_DHF_cases, forecast,horizon, offset1 ) 

out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'form'==as.character(form2))


  
preds <- out.list$log.samps.inc %>%
  reshape2::melt(., id.vars=c('date','district','horizon')) %>%
  group_by(date, district, horizon) %>%
  summarize(pred = exp(median(value)),
            lcl= exp(quantile(value, 0.025)),
            ucl=exp(quantile(value, 0.975))
            ) %>%
  left_join(c1.out, by=c('date', 'district')
            )



