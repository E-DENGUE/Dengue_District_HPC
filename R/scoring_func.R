scoring_func <- function(Y){
  
  forecast.index <- which((Y$ds$forecast==1))
  
  test1 <-inla.posterior.sample(1000, Y$mod,  num.threads=8,seed=123)
  
  pred.interval.func <- function(sample.ds, dist=c('nb','poisson')){
    mu1 <- exp(sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))])
    mu2 <- mu1[forecast.index]
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
  
  samps.inc <- apply(samps,2, function(x) x/Y$ds$pop[forecast.index]*100000)
  
  obs <- (Y$ds$m_DHF_cases/Y$ds$pop*100000)[forecast.index]

  miss.obs <- which(is.na(obs))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    samps.inc  <- samps.inc[-miss.obs,1,]
  }
  
  crps1 <- crps_sample(obs, samps.inc)
  
  #pred.samples.forecast.inc.me <- apply(samps.inc,1,median)
  return(crps1)
}

