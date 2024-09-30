source('./R/99_load.R')

#fit baseline through 2021, project to 2022
forecast_year=2012
district.select='CHO MOI'

for(i in 2012:2021){
  for(j in unique(d2$district)){
    ts_decomposition_inla(forecast_year=i, district.select=j)
  }
}



  #this function extracts the samples for the mean of lambda ('Predictor'), and then generates samples
  #from the predictive distribution using rpois or rnbinom
  
  pred.interval.func <- function(sample.ds, dist=c('nbinomial','poisson')){
    lambda0 <-  sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))]
    latent_t1 <-  sample.ds$latent[grep('time_id1:', row.names(sample.ds$latent), fixed=T)]
    #latent_t2 <-  sample.ds$latent[grep('time_id2:', row.names(sample.ds$latent), fixed=T)]
      endemic <- lambda0-latent_t1
    
    lambda1 <- exp(endemic)*c2$pop/100000 #"Predictor"=lambda/E 
    lambda2 <- lambda1
    if(dist=='nbinomial'){
      nb.size1 = sample.ds$hyperpar['size for the nbinomial observations (1/overdispersion)']
      pred <- replicate(10, rnbinom(n=length(lambda2), mu=lambda2, size=nb.size1), simplify = 'array')
    }else{
      pred <- replicate(10, rpois(n=length(lambda2), lambda=lambda2), simplify = 'array')
    }
    return(pred)
  }
  



  
  #Generate samples from the predictive distribution for the count
  
  test1 <-inla.posterior.sample(1000, mod1, seed=0)
  
  samps <- sapply(test1,pred.interval.func,dist=mod.family, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  #convert the count to incidence
  samps.inc <- apply(samps,2, function(x) x/c2$pop*100000)
  
  
  prob_epidemic <- NA
  for(i in 1:nrow(c2)){
    prob_epidemic[i] <- mean(c2$m_DHF_cases[i] > samps[i,])
  }

  cbind.data.frame('prob'=prob_epidemic[89:100],'cases'=c2$m_DHF_cases[89:100], 'prod'=c2$m_DHF_cases[89:100]*prob_epidemic[89:100]) %>%
    ggplot(aes(x=prob, y=cases , color=prod))+
    geom_point()+
    theme_classic()

  out_ds <- c2 %>%
    dplyr::select(date, district,  pop, m_DHF_cases)%>%
    mutate( pred_mean = apply(samps,1,mean),
            pred_lcl = apply(samps,1,quantile, probs=0.025),
            pred_ucl = apply(samps,1,quantile, probs=0.975))

  out_ds %>%
   # filter(district=='AN MINH') %>%
  ggplot(aes(x=date, y=m_DHF_cases) )+
    geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl))+
  geom_line(color='red')+
    theme_classic()
    
  
  
  #######################################################
  #######################################################
  ## Two equivalent approaches for getting the  difference between two discrete pdfs.
  library(sads)
  
  ###historical data has the frequency count from an empirical frequency distribution
  #estimated from an INLA time series decomposition model
  #this represents the mean and SD of the log-mean of the forecast for this time point.
  historic_log_mean =1
  historic_log_sd =0.5

  #estimated from the ensemble model
  #the range represents the min and max predicted count from the posterior samples of the ensemble
  forecast_log_mean =3
  forecast_log_sd =0.5
  forecast_range = c(0,200)
  test_values <- c(min(forecast_range):max(forecast_range))

  ## use dpoilog to get the probability of observing N cases for the each distribution
  historic_density <- dpoilog( test_values, mu=historic_log_mean, sig=historic_log_sd, log=F)
  forecast_density <- dpoilog( test_values, mu=forecast_log_mean, sig=forecast_log_sd, log=F)
  
  ##discrete pdf for the historical and forecasted values
  plot(test_values, historic_density, col='red', type='l')
  points(test_values, forecast_density, col='blue', type='l')
  abline(a=0, b=1)
  
  #difference between forecast density and observed density (positive area between these two curves)
  diff <- forecast_density - historic_density
  diff[diff<0] <- 0
  sum(diff)
  
  ##Alternative approach, with equivalent results involves resampling from each distribution
  
  set.seed(123)
  N_samp = 1000000
  historic <-  rpois(n=N_samp, lambda=exp(rnorm(N_samp,1,0.5))) %>%
    as.data.frame() %>%
    rename( N_cases='.') %>%
    group_by(N_cases) %>%
    summarize(freq_base = n())



  forecast_data <-  rpois(n=N_samp, lambda=exp(rnorm(N_samp,3,0.5))) %>%
    as.data.frame() %>%
    rename( N_cases='.') %>%
    group_by(N_cases) %>%
    summarize(freq_forecast = n())%>%
    left_join(historic, by='N_cases') %>% #do LEFT join, not full join--we don't care about integers where historic>forecast
    mutate(freq_base= if_else(is.na(freq_base),0, freq_base),
           prob_base= freq_base/sum(freq_base),
           prob_forecast=freq_forecast/sum(freq_forecast)
           )
    
  ggplot(forecast_data, aes(x=N_cases, y=prob_base))+
    geom_line(col='red') +
    geom_line(aes(x=N_cases, y=prob_forecast), col='blue')

  
  prob_diff <- forecast_data %>%
    mutate( diff_prob = prob_forecast - prob_base,
            diff_prob=if_else(diff_prob<0,0,diff_prob)) %>%
    summarize(diff_prob=sum(diff_prob))
  
  prob_diff

  