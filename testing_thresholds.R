source('./R/99_load.R')
library(sads)
library(tidyverse)

#fit baseline through 2021, project to 2022
forecast_year=2012
district.select='CHO MOI'

for(j in unique(d2$district)){
  print(j)
  for(i in 2012:2021){
    print(i)
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
  
  
  # baseline.files <-list.files('./Data/baselines',full.names =T)
  # 
  # all.baselines <- list()
  # for(i in 1:length(baseline.files)){
  #   print(i)
  #   all.baselines[[i]] <- readRDS(baseline.files[i])
  # }
  #all.baselines <- bind_rows(all.baselines)
  #saveRDS(all.baselines,'./Data/all_baselines.rds')
  
  all.baselines <- readRDS('./Data/all_baselines.rds')
  
  #DUC HOA, LONG PHU, My TU, AN MINH,AN PHU
  ds1 <-all.baselines %>%
   filter(district=='LONG PHU') %>%
    left_join(d2, by=c('date','district')) %>%
    mutate(ucl_baseline=NA, lcl_baseline=NA,prob_obs=NA,
           RR=m_DHF_cases/(exp(mean_log_baseline)*pop/100000))
    
  for(i in 1:nrow(ds1)){
    print(i)
    ds1$ucl_baseline[i] = quantile(rpois(100000,lambda=exp(rnorm(10000,(ds1$mean_log_baseline[i] + log(ds1$pop[i]/100000)),ds1$sd_log_baseline[i]))), probs=0.975)
    ds1$lcl_baseline[i] = quantile(rpois(100000,lambda=exp(rnorm(10000,(ds1$mean_log_baseline[i] + log(ds1$pop[i]/100000)),ds1$sd_log_baseline[i]))), probs=0.025)
    ds1$prob_obs[i]= ppoilog( ds1$m_DHF_cases[i] , mu=(ds1$mean_log_baseline[i] + log(ds1$pop[i]/100000)), sig=ds1$sd_log_baseline[i], log=F)
  }
    
  ds1 %>%
    ggplot() +
    geom_ribbon(aes( x=date,ymin=lcl_baseline, ymax=ucl_baseline), alpha=0.5)+
    geom_line(aes(x=date, y=exp(mean_log_baseline)*pop/100000), lty=2, col='red', lwd=0.75)+
    geom_point(aes(x=date, y=m_DHF_cases)) +
   theme_classic()
  
  preds <- readRDS( "./Data/cleaned_scores/all_crps_slim_updated_lag3.rds") %>% #note this is just for demo--not sure this is right version of output
   dplyr::select(date,vintage_date, district, pred_mean, horizon, pred_lcl, pred_ucl, modN) %>%
    filter(horizon==3 & modN=='mod1_')
  
  ds1.p <- ds1 %>%
    left_join(preds, by=c('district','date')) %>% 
    filter(!is.na(pred_mean)) 
  
    ggplot(ds1.p) +
      geom_ribbon(aes( x=date,ymin=0, ymax=exp(mean_log_baseline)*pop/100000),fill='#4daf4a', alpha=0.15)+
      geom_ribbon(aes( x=date,ymin=exp(mean_log_baseline)*pop/100000, ymax=ucl_baseline),fill='#ffff99', alpha=0.15)+
      geom_ribbon(aes( x=date,ymin=ucl_baseline, ymax=max(pred_ucl)
      ),fill='#e41a1c', alpha=0.15)+
      geom_line(aes( x=date,y=pred_lcl),col='black',lty=2)+
      geom_line(aes( x=date,y=pred_ucl),col='black',lty=2)+
      geom_line(aes( x=date,y=pred_mean),col='black',lty=1)+
      #geom_ribbon(aes( x=date,ymin=lcl_baseline, ymax=ucl_baseline), fill='#e41a1c',alpha=0.5)+
      geom_point(aes(x=date, y=m_DHF_cases)) +
      ylab("Cases") +
      theme_classic()

    ggplot(ds1.p) +
      geom_ribbon(aes( x=date,ymin=0, ymax=exp(mean_log_baseline)*pop/100000),fill='#4daf4a', alpha=0.15)+
      geom_ribbon(aes( x=date,ymin=exp(mean_log_baseline)*pop/100000, ymax=ucl_baseline),fill='#ffff99', alpha=0.15)+
      geom_ribbon(aes( x=date,ymin=ucl_baseline, ymax=max(pred_ucl)
      ),fill='#e41a1c', alpha=0.15)+
      geom_line(aes( x=vintage_date,y=pred_lcl),col='black',lty=2)+
      geom_line(aes( x=vintage_date,y=pred_ucl),col='black',lty=2)+
      geom_line(aes( x=vintage_date,y=pred_mean),col='black',lty=1)+
      #geom_ribbon(aes( x=date,ymin=lcl_baseline, ymax=ucl_baseline), fill='#e41a1c',alpha=0.5)+
      geom_point(aes(x=date, y=m_DHF_cases)) +
      ylab("Cases") +
      theme_classic()+
      ggtitle('Vintage Date')  
  
  
  
  ds1 %>%
    ggplot() +
    geom_line(aes(x=date, y=prob_obs), lty=1, col='red', lwd=0.75)+
    theme_classic()
  
  ds1 %>%
    ggplot() +
    geom_point(aes(x=m_DHF_cases, y=prob_obs),  col='red')+
    geom_hline(yintercept=0.5, lty=2)+
    ylab("Probability that observed is greatert han historical")+
    theme_classic()
  
  ds1 %>%
    ggplot() +
    geom_point(aes(x=m_DHF_cases, y=RR),  col='red')+
    geom_hline(yintercept=0.5, lty=2)+
    ylab("Ratio of Observed vs historical mean")+
  theme_classic()
  
  
  ds1.plot <- ds1 %>%
    mutate(RR_LCL= m_DHF_cases/lcl_baseline,
           RR_UCL= m_DHF_cases/ucl_baseline,
           
           RD = m_DHF_cases - (exp(mean_log_baseline)*pop/100000),
           RD_LCL = m_DHF_cases - lcl_baseline,
           RD_UCL = m_DHF_cases - ucl_baseline,
           )
  
  ds1.plot %>% ggplot() +
    # Adding colored background bands
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -Inf, ymax = 1), fill = "#4daf4a", alpha = 0.2) +
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 1.5, ymax = 2.5), fill = "#ffff99", alpha = 0.2) +
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 2.5, ymax = Inf), fill = "#e41a1c", alpha = 0.2) +
    #add the data
    geom_ribbon(aes( x=date,ymin=RR_LCL, ymax=RR_UCL), alpha=0.5)+
    geom_line(aes(x=date, y=RR), lty=2, col='red', lwd=0.75)+
    geom_hline(yintercept=1) +
    theme_classic()
  
  ds1.plot %>% ggplot() +
    # Adding colored background bands
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -Inf, ymax = 5), fill = "#4daf4a", alpha = 0.2) +
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 5, ymax = 25), fill = "#ffff99", alpha = 0.2) +
    geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 25, ymax = Inf), fill = "#e41a1c", alpha = 0.2) +
    #add the data
    geom_ribbon(aes( x=date,ymin=RD_LCL, ymax=RD_UCL), alpha=0.5)+
    geom_line(aes(x=date, y=RD), lty=2, col='red', lwd=0.75)+
    geom_hline(yintercept=1) +
    theme_classic()
  
  ds1.plot %>%
    ggplot(aes(x=RR, y=RD))+
    geom_point()+
    theme_classic()+
    geom_hline(yintercept=1) +
    geom_vline(xintercept=1) 
    
  #Fold change above baseline mean vs probability that the observation is greater than historical range
 p1 <- ds1 %>%
    ggplot() +
    geom_point(aes(x=RR, y=prob_obs, color=m_DHF_cases))+
    geom_vline(xintercept=1, lty=2)+
    geom_hline(yintercept=0.5, lty=2)+
    scale_color_viridis_c() +  # Apply viridis palette
    theme_classic()+
    ylab("Probability that observed is greatert than historical")+
    xlab("Ratio of Observed vs historical mean")
 p1 
 plotly::ggplotly(p1)
 
 
 p1 <- ds1 %>%
   ggplot() +
   geom_point(aes(x=log(m_DHF_cases+1), y=prob_obs))+
   geom_vline(xintercept=1, lty=2)+
   geom_hline(yintercept=0.5, lty=2)+
   scale_color_viridis_c() +  # Apply viridis palette
   theme_classic()+
   ylab("Probability that observed is greatert than historical")+
   xlab("log-Number of observed cases")
 p1 
 
  dist1 <- rpois(100000,lambda=exp(rnorm(10000,5,0.5)))
  dist2 <- rpois(100000,lambda=exp(rnorm(10000,8,0.5)))
  dist.comp<- cbind.data.frame(dist1, dist2)
    
    ggplot(dist.comp) +
    geom_histogram(aes(x=dist1),alpha=0.5, fill='red')+
    geom_histogram(aes(x=dist2), alpha=0.5,fill='blue')+
      theme_classic()
      
   
 #these are equivalent
  #qpoilog(p=0.975,5, 1)
  #quantile(rpois(100000,lambda=exp(rnorm(10000,5,1))), probs=0.975)
    
    #quantile(rpois(100000,lambda=exp(rnorm(10000,4.6,0.24))), probs=0.975)
    
      #######################################################
  #######################################################
  ## Two equivalent approaches for getting the  difference between two discrete pdfs.
  library(sads)
    
  
  ###historical data has the frequency count from an empirical frequency distribution
  #estimated from an INLA time series decomposition model
  #this represents the mean and SD of the log-mean of the forecast for this time point.
  historic_log_mean =2
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
  N_samp = 100000

   forecast1 <-  rpois(n=N_samp, lambda=exp(rnorm(N_samp,forecast_log_mean,forecast_log_sd)))
  historic1 <-  rpois(n=N_samp, lambda=exp(rnorm(N_samp,historic_log_mean,historic_log_sd)))
  
  mean((forecast1 - historic1)>0) 
  
  ks.test(historic1, forecast1, alternative='greater')
  wilcox.test(historic1, forecast1, alternative='greater')
  
  ##################################################
  ##################################################
  ## Test range
  
  #this represents the mean and SD of the log-mean of the forecast for this time point.
  dens_func <- function(historic_log_mean=3,
                        historic_log_sd =0.5, 
                        forecast_log_mean =3,
                        forecast_log_sd =0.5){
 
      #estimated from the ensemble model
      #the range represents the min and max predicted count from the posterior samples of the ensemble
      
      forecast_range = c(0,200)
      test_values <- c(min(forecast_range):max(forecast_range))
      
      ## use dpoilog to get the probability of observing N cases for the each distribution
      historic_density <- dpoilog( test_values, mu=historic_log_mean, sig=historic_log_sd, log=F)
      forecast_density <- dpoilog( test_values, mu=forecast_log_mean, sig=forecast_log_sd, log=F)
      
      
      historic_samp_mu <- rnorm( 10000, mean=historic_log_mean, sd=historic_log_sd)
      historic_samp <- rpois( 10000, lambda=exp(historic_samp_mu))
      
      #historic_samp <- rpoilog( 10000, mu=historic_log_mean, sig=historic_log_sd)
      
      forecast_samp_mu <- rnorm(10000,  mean=forecast_log_mean, sd=forecast_log_sd)
      forecast_samp <- rpois(10000,  lambda=exp(forecast_samp_mu))
      
      
      ##discrete pdf for the historical and forecasted values
      plot.ds <- cbind.data.frame(test_values,historic_density,forecast_density) %>%
        mutate(shade_min = if_else(forecast_density>historic_density,historic_density,forecast_density )) %>%
        ggplot()+
        geom_line(aes(x=test_values, y=historic_density), col='red')+
        geom_line(aes(x=test_values, y=forecast_density), col='blue')+
        theme_classic() +
        geom_ribbon(aes(x=test_values, ymin=shade_min, ymax=forecast_density), alpha=0.1, fill='blue')


      #difference between forecast density and observed density (positive area between these two curves)
      diff <- forecast_samp - historic_samp
      p_greater <- mean(diff>0)
      p_greater
      
      p_indiv <-  sapply(forecast_samp, function(x){mean(x>historic_samp) }  )
      mean(p_indiv)
      
      
      out=list('prob'=p_greater,'plot1'=plot.ds , 'risk'= exp(forecast_log_mean)*(p_greater))
  }
  
  call.func <- dens_func(historic_log_mean=2,
            historic_log_sd =0.5, 
            forecast_log_mean =4,
            forecast_log_sd =0.5)
  call.func
  
  historic_log_means <- rep(seq(0.5,6,by=0.25), times=24)
  forecast_log_means <-rep(seq(0.5,6,by=0.25), each=24)
  
  test_range <- mapply(dens_func, historic_log_mean=historic_log_means, forecast_log_mean=forecast_log_means, SIMPLIFY =F)
  
  probs_greater <- sapply(test_range,'[[','prob')
  risks <- sapply(test_range,'[[','risk')
  
  pred_ds <- cbind.data.frame(probs_greater, risks,historic_log_means,forecast_log_means)
  
  ggplot(pred_ds) +
    geom_point(aes(x=log(forecast_log_means/historic_log_means), y=risks, color=probs_greater))+
    xlab('log(Rate ratio)')+
    ylab('Risk score') +
    theme_classic()+
    geom_vline(xintercept=0)
  
  ggplot(pred_ds) +
    geom_point(aes(x=forecast_log_means, y=risks, color=probs_greater))+
    xlab('Forecasted_rate')+
    ylab('Risk score') +
    theme_classic()
  