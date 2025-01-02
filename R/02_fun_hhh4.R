hhh4_mod <- function(vintage_date, modN,max_horizon=3){
  
  sim.mat <- readRDS('./Data/tsclust_simmat.rds')
  
  
  MDR_NEW <- readRDS( "./Dengue_District_HPC-predict_3mo/Data/MDR_NEW.rds") %>%
    arrange(ID)
  
  row.names(MDR_NEW) <- MDR_NEW$VARNAME
  
  neighb <- surveillance::poly2adjmat(st_make_valid(MDR_NEW))
  
  dist_nbOrder <- nbOrder(neighb)
  
  colnames(dist_nbOrder) <- MDR_NEW$VARNAME
  
  map1 <- sf:::as_Spatial(MDR_NEW)
  
  c1 <- d2 %>%
    arrange(district, date) %>%
    filter( date>='2004-09-01' & date <= (as.Date(vintage_date)  %m+% months(max_horizon))) %>%
    group_by(district) %>%
    mutate( log_inc=log((m_DHF_cases+1)/pop*100000),
            log_lag12_inc= scale(dplyr::lag(log_inc,12) )[,1] ) %>%
    ungroup()
  
  start.date <- min(c1$date)
  start.year <- lubridate::year(start.date)
  start.week <- lubridate::week(start.date)
  start.month <- lubridate::month(start.date)
  
  c1.fit <- c1 %>% 
    filter( date <= (vintage_date %m+% months(max_horizon))) %>%
    mutate(m_DHF_cases_fit = ifelse(date > vintage_date, NA_real_,m_DHF_cases))
  
  cases <- c1.fit %>% 
    reshape2::dcast(date~district, value.var= 'm_DHF_cases') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  cases.fit <- c1.fit %>% 
    reshape2::dcast(date~district, value.var= 'm_DHF_cases_fit') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  pop <- c1.fit %>% 
    mutate(pop2=pop/100000) %>%
    reshape2::dcast(date~district, value.var= 'pop2') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  temp_lag3 <- c1.fit %>% 
    mutate(lag3_avg_daily_temp = scale(lag3_avg_daily_temp)) %>%
    reshape2::dcast(date~district, value.var= 'lag3_avg_daily_temp') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  precip_lag3 <- c1.fit %>% 
    mutate(lag3_monthly_cum_ppt = scale(lag3_monthly_cum_ppt)) %>%
    reshape2::dcast(date~district, value.var= 'lag3_monthly_cum_ppt') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  
  log_cum_inc_24m <- c1.fit %>% 
    mutate(log_cum_inc_24m = scale(log_cum_inc_24m)) %>%
    reshape2::dcast(date~district, value.var= 'log_cum_inc_24m') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  log_lag12_inc <- c1.fit %>% 
    mutate(log_lag12_inc = scale(log_lag12_inc)) %>%
    reshape2::dcast(date~district, value.var= 'log_lag12_inc') %>%
    filter(date>=start.date) %>%
    dplyr::select(unique(MDR_NEW$VARNAME))%>%
    as.matrix()
  
  
  
  #unique(MDR_NEW$VARNAME) == colnames(pop)
  
  #Define STS object
  dengue_df <- sts(cases.fit, start = c(start.year, start.month), frequency = 12,
                   population = pop, neighbourhood = dist_nbOrder, map=map1)
  
  #sim.mat <- as.matrix(sim.mat)
  
  sim.mat2 <- `dim<-`(c(sim.mat), dim(sim.mat))
  
  colnames(sim.mat2) <- MDR_NEW$VARNAME
  
  
  dengue_df_dist <- sts(cases.fit, start = c(start.year, start.month), frequency = 12,
                        population = pop, neighbourhood =sim.mat2 , map=map1)
  
  all_dates <- sort(unique(c1$date))
  
  last_fit_t = which(all_dates == vintage_date )
  
  mod.select = mods[as.numeric(modN)]
  
  if(mod.select=='hhh4_np'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t + ri() , period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 + ri() ),
      ne = list(f = ~ -1 + temp_lag3 + ri() , weights = W_np(maxlag = 2)),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3)
    )
  } else if(mod.select=='hhh4_power'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t + ri() , period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 + ri() ),
      ne = list(f = ~ -1 + temp_lag3 + ri() , weights =  W_powerlaw(maxlag = 5)),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3)
    )
  } else if(mod.select=='hhh4_power_precip_temp'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t + ri() , period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 + precip_lag3 + ri() ),
      ne = list(f = ~ -1 + temp_lag3 + precip_lag3 + ri() , weights =  W_powerlaw(maxlag = 5)),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3,precip_lag3=precip_lag3)
    )
  }else if(mod.select=='hhh4_basic'){ 
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t + ri(), period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 + ri() , lag=1),
      ne = list(f = ~ -1 + temp_lag3 + ri() , weights = neighbourhood(dengue_df) == 1, lag=1),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3)
    )
  }else if(mod.select=='hhh4_power_precip_temp_endmc1'){ 
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t +temp_lag3 +precip_lag3+ ri(), period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 + precip_lag3+ ri() , lag=1),
      ne = list(f = ~ -1 + temp_lag3 + precip_lag3+ ri() , weights = neighbourhood(dengue_df) == 1, lag=1),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3,precip_lag3=precip_lag3)
    )
  }else if(mod.select=='hhh4_power_precip_temp_endmc2'){ 
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t +temp_lag3 +precip_lag3+ ri(), period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 +  ri() , lag=1),
      ne = list(f = ~ -1 +  ri() , weights = neighbourhood(dengue_df) == 1, lag=1),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3,precip_lag3=precip_lag3)
    )
    
  } else if(mod.select=='hhh4_power_cum_lag24'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t +  log_cum_inc_24m +  ri() , period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 +   ri() +log_cum_inc_24m), #log_lag12_inc not associated with AR
      ne = list(f = ~ -1 + temp_lag3 +  log_cum_inc_24m +  ri() , weights =  W_powerlaw(maxlag = 5)),
      family = "NegBin1",
      subset = 24:last_fit_t,
      data=list(temp_lag3=temp_lag3,log_cum_inc_24m=log_cum_inc_24m)
    )
  } else if(mod.select=='hhh4_power_lag12'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t +  log_lag12_inc +  ri() , period = dengue_df@freq),
                 offset = population(dengue_df)),
      ar = list(f = ~ -1 + temp_lag3 +   ri() ), #log_lag12_inc not associated with AR
      ne = list(f = ~ -1 + temp_lag3 +  log_lag12_inc +  ri() , weights =  W_powerlaw(maxlag = 5)),
      family = "NegBin1",
      subset = 13:last_fit_t,
      data=list(temp_lag3=temp_lag3,log_lag12_inc=log_lag12_inc)
    )
  } else if(mod.select=='hhh4_power_precip_temp_dist'){
    dengue_mod_ri_temp <- list(
      end = list(f = addSeason2formula(~ -1 + t + ri() , period = dengue_df_dist@freq),
                 offset = population(dengue_df_dist)),
      ar = list(f = ~ -1 + temp_lag3 + precip_lag3 + ri() ),
      ne = list(f = ~ -1 + temp_lag3 + precip_lag3 + ri() , weights =  W_powerlaw(maxlag = 5)),
      family = "NegBin1",
      subset = 2:last_fit_t,
      data=list(temp_lag3=temp_lag3,precip_lag3=precip_lag3)
    )
  }
  
  #fit the model to time t
  dengueFit_ri <- hhh4(stsObj = dengue_df, control = dengue_mod_ri_temp)
  
  last.y <- observed(dengue_df)[last_fit_t,]
  
  #simulate forward
  dengueSim <- simulate(dengueFit_ri,
                        nsim = 999, seed = 1, subset = (last_fit_t+1):(last_fit_t+max_horizon),
                        y.start = last.y)
  
  plot(dengueSim, type = "time", average = median)
  
  #par(mfrow = c(1,1), mar = c(3, 5, 2, 1), las = 1)
  # plot(dengueFit_ri, type = "fitted", total = TRUE,
  #      hide0s = TRUE, par.settings = NULL, legend = FALSE)
  # plot(dengueSim, "fan", means.args = list(), key.args = list(), add=T)
  
  samps <- dengueSim[(max_horizon-2):max_horizon,,] #samples from fitted model
  
  pop_forecast <- population(dengue_df)[(last_fit_t+max_horizon-2):(last_fit_t+max_horizon),] #NOTE THIS IS ALREADY DIVIDED BY 100,000
  obs_forecast <- cases[(last_fit_t+max_horizon-2):(last_fit_t+max_horizon),]   
  
  samps.inc <- array(apply(samps,3, function(x) x/pop_forecast), dim=dim(samps))
  
  #Log(Incidence)
  log.samps.inc <- array(apply(samps,3, function(x) log((x+1)/pop_forecast) ), dim=dim(samps))
  
  log.samps.inc_mean <-apply(log.samps.inc,1,mean)
  
  obs_inc <- obs_forecast/pop_forecast
  log_obs_inc <- log((obs_forecast+1)/pop_forecast)
  
  #combine the CRPS scores with the 95% posterior predictive distribution (equal tailed)
  forecast_ds <- c1 %>%
    filter(date == vintage_date + months(1)|date == vintage_date + months(2)|date == vintage_date + months(3))
  
  out_ds <- forecast_ds %>%
    dplyr::select(date, district,   pop, m_DHF_cases)%>%
    mutate( forecast=1,
            horizon =  if_else(
              date == (vintage_date + months(1)), 1,
              if_else(
                date == (vintage_date + months(2)), 2,
                if_else(
                  date == (vintage_date + months(3)), 3,
                  0
                )
              )
            )
    )
  
  
  
  
  
  crps3 <- data.frame()
  c1.out <- data.frame()
  samps_out_list <- list()
  
  for (h in 1:max_horizon) {
    samps <- matrix(dengueSim[h,,], nrow=dim(dengueSim)[2])
    
    pop_forecast <- population(dengue_df)[last_fit_t+h,]
    obs_forecast <- cases[last_fit_t+h,]
    
    samps.inc <- apply(samps, 2, function(x) x / pop_forecast)
    log.samps.inc <- log(apply(samps, 2, function(x) (x + 1) / pop_forecast))
    log.samps.inc_mean <- apply(log.samps.inc, 1, mean)
    
    obs_inc <- obs_forecast / pop_forecast
    log_obs_inc <- log((obs_forecast + 1) / pop_forecast)
    
    # Create forecast data frame
    forecast_ds <- c1 %>%
      filter(date == vintage_date + months(h))
    
    out_ds <- forecast_ds %>%
      dplyr::select(date, district, pop, m_DHF_cases) %>%
      mutate(
        forecast = 1,
        horizon = h,
        pred_mean = apply(samps, 1, mean),
        pred_lcl = apply(samps, 1, quantile, probs = 0.025),
        pred_ucl = apply(samps, 1, quantile, probs = 0.975)
      )
    
    # Calculate CRPS scores
    crps1 <- crps_sample(obs_inc, samps.inc)
    crps2 <- crps_sample(log_obs_inc, log.samps.inc)
    
    # Combine CRPS scores and forecast data
    crps3 <- cbind.data.frame(crps1, crps2,out_ds)    
    
    # Prepare c1.out for this horizon
    vintage_date <- as.Date(vintage_date)
    c1b <- c1 %>%
      ungroup() %>%
      mutate(
        forecast = if_else(date == (vintage_date %m+% months(h)), 1, 0),
        horizon = if_else(date == (vintage_date %m+% months(h)), h, 0)
      ) 
    
    
    c1.out <- c1b %>%
      dplyr::select(date, district, m_DHF_cases,pop, forecast,horizon ) 
    
    samps.out <- cbind.data.frame('date'=out_ds$date, 'district'=out_ds$district, 'horizon'=out_ds$horizon, log.samps.inc)
    
    out.list =  list ('ds'=c1.out, 'scores'=crps3,'log.samps.inc'=samps.out)
    samps_out_list[[h]] <-  out.list
  }
  
  
  # rbind lists by matching names
  combine_lists <- function(all_lists) {
    names_list <- unique(unlist(lapply(all_lists, names)))
    combined <- lapply(names_list, function(name) {
      do.call(rbind, lapply(all_lists, function(lst) lst[[name]]))
    })
    names(combined) <- names_list
    combined
  }
  
  out.list <- combine_lists(samps_out_list)
  
  # Show the combined list
  #out.list
  
  c1.out<- out.list$ds
  crps3<- out.list$scores
  samps.out<- out.list$log.samps.inc
  
  out.list =  list ('ds'=c1.out, 'scores'=crps3,'log.samps.inc'=samps.out)
  
  saveRDS(out.list,paste0('./Results/Results_hhh4/', 'mod',mod.select,'_',vintage_date  ,'.rds' )   )
  
  return(out.list)
}

