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
  
  samps.inc <- apply(samps,2, function(x) x/forecast_ds$pop*100000)
  
  obs <- (forecast_ds$m_DHF_cases/forecast_ds$pop*100000)

  miss.obs <- which(is.na(obs))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    samps.inc  <- samps.inc[-miss.obs,1,]
  }
  
  crps1 <- crps_sample(obs, samps.inc)
  
  crps1 <- cbind.data.frame(crps1, forecast_ds) 
  
  #pred.samples.forecast.inc.me <- apply(samps.inc,1,median)
  return(crps1)
}

