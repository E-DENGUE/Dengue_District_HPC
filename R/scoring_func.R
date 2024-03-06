scoring_func <- function(Y){
  
  forecast_ds <- Y$ds %>%
         mutate(index=row_number()) %>%
         filter(forecast==1 )
         
         
  forecast.index_horizon1 <- forecast_ds %>%
         filter(forecast==1 & horizon==1) %>%
         pull(index)
         
  forecast.index_horizon2 <- forecast_ds %>%
         filter(forecast==1 & horizon==2) %>%
         pull(index)
         
  forecast.index1 <- sort(c(forecast.index_horizon1,forecast.index_horizon2))

  test1 <-inla.posterior.sample(1000, Y$mod,  num.threads=8,seed=123)
  
  pred.interval.func <- function(sample.ds, dist=c('nb','poisson')){
    mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
    mu2 <- mu1[forecast.index1]
    if(dist=='nb'){
      nb.size1 = sample.ds$hyperpar['size for the nbinomial observations (1/overdispersion)']
      pred <- replicate(10, rnbinom(n=length(mu2), mu=mu2, size=nb.size1), simplify = 'array')
    }else{
      pred <- replicate(10, rpois(n=length(mu2), lambda=mu2), simplify = 'array')
    }
    return(pred)
  }
  
  samps <- sapply(test1,pred.interval.func,dist=Y$mod.family, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  #This verifies that SAMPSis on the scale of cases
  #samps.mean <- apply(samps,1,mean)
  #plot(samps.mean,forecast_ds$m_DHF_cases )
  #abline(a=0, b=1)
  #sum(samps.mean)
  #sum(forecast_ds$m_DHF_cases)
  
  samps.inc <- apply(samps,2, function(x) x/forecast_ds$pop*100000)
  log.samps.inc <- log(apply(samps,2, function(x) (x+1)/forecast_ds$pop*100000))
  
  log.samps.inc_mean <-apply(log.samps.inc,1,mean)
  
  obs_inc <- (forecast_ds$m_DHF_cases/forecast_ds$pop*100000)
  log_obs_inc <- log((forecast_ds$m_DHF_cases+1)/forecast_ds$pop*100000)
    
  miss.obs <- which(is.na(obs))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    samps.inc  <- samps.inc[-miss.obs,1,]
  }
  
  crps1 <- crps_sample(obs_inc, samps.inc)

  crps2 <- crps_sample(log_obs_inc, log.samps.inc) #on the log scale, as recommended by https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1011393#sec008
  
    
  crps3 <- cbind.data.frame(crps1, crps2,forecast_ds) 
  
  #pred.samples.forecast.inc.me <- apply(samps.inc,1,median)
  return(crps3)
}

