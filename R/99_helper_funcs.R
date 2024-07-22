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