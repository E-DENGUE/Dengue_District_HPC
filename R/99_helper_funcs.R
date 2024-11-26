#Helper functions
## scoring_func()
## deseasonalize_climate()
## predict.rlm()


#############################
## Scoring function
#############################
scoring_func <- function(Y){
  
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
  
  pred.interval.func <- function(sample.ds, dist=c('nb','poisson')){
    lambda1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])*in.ds$pop/100000 #"Predictor"=lambda/E 
    lambda2 <- lambda1[forecast.index1]
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

############################
##deseasonalize_climate()
############################

deseasonalize_climate <- function(climate_var, ds=d1){
  
  seas.mod <-ds %>% 
    arrange(district, date) %>%
    group_by(district) %>%
    mutate( Climate_Train = if_else(date<as.Date('2005-01-01'), .data[[climate_var]], NA_real_),
            t=row_number(),
            sin12=sin(2*pi*t/12),
            cos12=cos(2*pi*t/12),
            sin6=sin(2*pi*t/6),
            cos6=cos(2*pi*t/6),
    )  %>%
    ungroup()
  
  form1 <-as.formula(paste0('Climate_Train', '~ sin12 + cos12+ sin6 +cos6'))
  
  fitted_models = seas.mod %>% 
    group_by(district) %>% 
    do(mod1 = rlm(form1, data=.))
  
  
  fun1 <- function(X, Y){
    seas.mod %>% filter(district==X) %>%
      cbind.data.frame(., predict.rlm( Y, newdata = seas.mod[seas.mod$district==X,], interval = "prediction"))
  }
  
  all_preds <-  mapply( fun1, fitted_models$district, fitted_models$mod1, SIMPLIFY=F) %>%
    bind_rows()
  
  all_mods <- all_preds %>%
    mutate(climate_diff = (.data[[climate_var]] - upr),
           climate_aberration = if_else(.data[[climate_var]] > upr,climate_diff , 0 ) 
    ) %>% 
    dplyr::select(district,district, date,climate_aberration)
  
  return(all_mods)
}  

#######################################
###PREDICT.RLM
predict.rlm <- function (object, newdata = NULL, scale = NULL, ...)
{
  ## problems with using predict.lm are the scale and
  ## the QR decomp which has been done on down-weighted values.
  object$qr <- qr(sqrt(object$weights) * object$x)
  predict.lm(object, newdata = newdata, scale = object$s, ...)
}

ts_decomposition_inla <- function(forecast_year=2012, district.select='CHO MOI'){

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
             lag_y = lag(log_df_rate, 1),
             lag2_y = lag(log_df_rate, 2),
             lag3_y = lag(log_df_rate, 3),
            
             sin12 = sin(2*pi*t/12),
             cos12 = cos(2*pi*t/12),
             month=as.factor(month(date)),
             monthN=month(date),
             offset1 = pop/100000,
             #log_offset=log(pop/100000)
      ) %>%
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
        yearN= as.numeric(as.factor(year)),
        
        urban_dic = as.factor(if_else(Urbanization_Rate>=40,1,0))) %>%
      arrange(date,districtID) %>% #SORT FOR SPACE_TIME
      mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
             timeIDpad=str_pad(time_id1, 5, pad = "0", side='left')
       
      )
    
    c2 <- c1 %>%
      filter(district==district.select & year <=forecast_year) %>%
      mutate(m_DHF_cases_fit = if_else(year>=forecast_year, NA_real_, m_DHF_cases))
    
    offset1 <- c2$offset1
    
    #single district version
    form2 <- as.formula( 'm_DHF_cases_fit ~ 1+
            f(time_id1, model="ar1",constr=TRUE) +
            f(yearN, model="rw1", hyper=hyper2.rw,  constr=TRUE)+
            f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
    )
    
    n_times <- length(unique(c2$time_id1))
    n_years <- length(unique(c2$year))
    
    pred.time_id1 <- c2$time_id1[(n_times-11):n_times]
    pred.yearN <- c2$yearN[(n_times-11):n_times]
    pred.monthN <- c2$monthN[(n_times-11):n_times]
    
    time.mat <- model.matrix(~ -1 + as.factor(time_id1) , data=c2)
    year.mat <- model.matrix(~ -1 + as.factor(yearN) , data=c2)
    month.mat <- model.matrix(~ -1 + as.factor(monthN) , data=c2)
    
    #confirm linear combs gives same results as summary.linear.predictor
    # lc.lin.pred = inla.make.lincombs("(Intercept)"=rep(1,12),
    #                              'time_id1' = time.mat[(n_times-11):n_times,], 
    #                              'yearN' = year.mat[(n_times-11):n_times,],
    #                              'monthN'=month.mat[(n_times-11):n_times,])
    
    lc.lin.pred.no.ar1 = inla.make.lincombs("(Intercept)"=rep(1,nrow(c2)),
                                     'time_id1' = rep(0,nrow(c2)), 
                                     'yearN' = year.mat,
                                     'monthN'=month.mat)
    
    mod1 <- inla(form2, data = c2,  family = "poisson",E=offset1,
                 lincomb = lc.lin.pred.no.ar1,
                 control.compute = list(dic = FALSE, 
                                        waic = FALSE, 
                                        config = T,
                                        return.marginals=F
                 ),
                 # save predicted values on response scale
                 control.predictor = list(compute=TRUE, link=1),
                 control.fixed = list(mean.intercept=0, 
                                      prec.intercept=1e-4, # precision 1
                                      mean=0, 
                                      prec=1), # weakly regularising on fixed effects (sd of 1)
                 inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
                 num.threads=8
    )    
    mod.family <- mod1$.args$family
    
    #need the linear predictor minus the AR(1) using lincomb; ar(1) random effect should be uncorrelated from rest of model;
    lin.pred.no.ar1 <- mod1$summary.lincomb.derived[,c('mean','sd')] %>%
      rename(mean.no.ar1=mean, sd.no.ar1=sd)
    
    ##dataset with the baseline expected values!
    baseline <- cbind.data.frame('date'=c2$date ,lin.pred.no.ar1) %>%
      rename(mean_log_baseline= mean.no.ar1, sd_log_baseline=sd.no.ar1) %>%
      mutate(year=lubridate::year(date),
             district=district.select) %>%
      filter(year==forecast_year) %>%
      saveRDS( paste0('./Data/baselines/baseline_', forecast_year,'_',district.select,'.rds'))
        
    # lin.pred <- mod1$summary.linear.predictor[,c('mean','sd')]%>%
        #   rename(mean.lin.pred=mean, sd.lin.pred=sd) 
        # 
        # comb.pred <- cbind.data.frame('date'=c2$date ,lin.pred.no.ar1, lin.pred) %>%
        #   mutate(t=row_number())
        # 
        # ggplot(comb.pred, aes(x=date, y=mean.no.ar1))+
        #   geom_line(color='red')+
        #   geom_line(aes(x=date, y=mean.lin.pred))
}



## SEASONAL DECOMPOSITION IN INLA TO GET THE BASELINE
ts_decomposition_inla <- function(forecast_year=2012, district.select='CHO MOI'){
  
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
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           lag3_y = lag(log_df_rate, 3),
           
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
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
      yearN= as.numeric(as.factor(year)),
      
      urban_dic = as.factor(if_else(Urbanization_Rate>=40,1,0))) %>%
    arrange(date,districtID) %>% #SORT FOR SPACE_TIME
    mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
           timeIDpad=str_pad(time_id1, 5, pad = "0", side='left')
           
    )
  
  c2 <- c1 %>%
    filter(district==district.select & year <=forecast_year) %>%
    mutate(m_DHF_cases_fit = if_else(year>=forecast_year, NA_real_, m_DHF_cases))
  
  offset1 <- c2$offset1
  
  #single district version
  form2 <- as.formula( 'm_DHF_cases_fit ~ 1+
            f(time_id1, model="ar1",constr=TRUE) +
            f(yearN, model="rw2", constr=TRUE)+
            f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
  )
  
  n_times <- length(unique(c2$time_id1))
  n_years <- length(unique(c2$year))
  
  pred.time_id1 <- c2$time_id1[(n_times-11):n_times]
  pred.yearN <- c2$yearN[(n_times-11):n_times]
  pred.monthN <- c2$monthN[(n_times-11):n_times]
  
  time.mat <- model.matrix(~ -1 + as.factor(time_id1) , data=c2)
  year.mat <- model.matrix(~ -1 + as.factor(yearN) , data=c2)
  month.mat <- model.matrix(~ -1 + as.factor(monthN) , data=c2)
  
  #confirm linear combs gives same results as summary.linear.predictor
  # lc.lin.pred = inla.make.lincombs("(Intercept)"=rep(1,12),
  #                              'time_id1' = time.mat[(n_times-11):n_times,], 
  #                              'yearN' = year.mat[(n_times-11):n_times,],
  #                              'monthN'=month.mat[(n_times-11):n_times,])
  
  lc.lin.pred.no.ar1 = inla.make.lincombs("(Intercept)"=rep(1,nrow(c2)),
                                          'time_id1' = rep(0,nrow(c2)), 
                                          'yearN' = year.mat,
                                          'monthN'=month.mat)
  
  mod1 <- inla(form2, data = c2,  family = "poisson",E=offset1,
               lincomb = lc.lin.pred.no.ar1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, link=1),
               control.fixed = list(mean.intercept=0, 
                                    prec.intercept=1e-4, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=8
  )    
  mod.family <- mod1$.args$family
  
  #need the linear predictor minus the AR(1) using lincomb; ar(1) random effect should be uncorrelated from rest of model;
  lin.pred.no.ar1 <- mod1$summary.lincomb.derived[,c('mean','sd')] %>%
    rename(mean.no.ar1=mean, sd.no.ar1=sd)
  
  ##dataset with the linear predictor
  baseline <- cbind.data.frame('date'=c2$date ,mod1$summary.linear.predictor) %>%
    rename(mean_log_baseline= mean, sd_log_baseline=sd) %>%
    mutate(year=lubridate::year(date),
           district=district.select) %>%
    filter(year==forecast_year) %>%
    saveRDS( paste0('./Data/baselines/baseline_', forecast_year,'_',district.select,'.rds'))
  
  # lin.pred <- mod1$summary.linear.predictor[,c('mean','sd')]%>%
  #   rename(mean.lin.pred=mean, sd.lin.pred=sd) 
  # 
  # comb.pred <- cbind.data.frame('date'=c2$date ,lin.pred.no.ar1, lin.pred) %>%
  #   mutate(t=row_number())
  # 
  # ggplot(comb.pred, aes(x=date, y=mean.no.ar1))+
  #   geom_line(color='red')+
  #   geom_line(aes(x=date, y=mean.lin.pred))
}
