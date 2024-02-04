source('./R/load.R')
  date.test.in <- as.Date('2014-02-01')
  modN=4
  formula1 ='m_DHF_cases_hold~   lag2_y +
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
  
  
  c1 <- d2 %>%
    # filter(district %in% select.districts) %>%
    arrange(district, date) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *100000,
           log_df_rate = log((m_DHF_cases +1)/ pop *100000) , 
           log_pop=log(pop/100000),
           year = year(date) ,
           m_DHF_cases_hold= if_else( date>= (date.test.in[1]), NA_real_,
                                      m_DHF_cases),
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           
           t=row_number(),
           t2=t,
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (date.test.in[1] %m+% months(1) ) & !is.na(lag2_y)) %>% #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(districtID = as.numeric(as.factor(district)),
           districtID2 = districtID,
           districtID3 = districtID)
  
  form2 <- as.formula (formula1)
  
  #View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y)  %>% filter(date>=as.Date('2012-01-01')))

    # form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  #nbinomial or poisson
  
  offset1 <- c1$offset1
  mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, 
                                        link=1),
               control.inla = list(strategy='adaptive', # adaptive gaussian
                                   cmin=0),
               control.fixed = list(mean.intercept=0, 
                                    prec.intercept=1, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=8
  )    
  
  
  mod.family <- mod1$.args$family
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
           horizon = if_else(date== (date.test.in[1]),1,
                             if_else(date== (date.test.in[1] %m+% months(1)),2, 0
                             )
           ),
           max_allowed_lag = if_else(grepl('lag_y',formula1 ),1,2)
           )%>%
    filter(horizon <= max_allowed_lag) #get rid of lag2 if lag1 is included as covariate
  #View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)
  
  Y = score.list
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
  
  #pop.check <- c1$pop[forecast.index1]
  #plot(pop.check,forecast_ds$pop )
  
  samps <- sapply(test1,pred.interval.func,dist=Y$mod.family, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  samps.inc <- apply(samps,2, function(x) x/forecast_ds$pop*100000)
  
  obs <- (forecast_ds$m_DHF_cases/forecast_ds$pop*100000)
  
  plot(samps.inc[,1] ,obs)
  plot(log(samps.inc[,2]+1) ,log(obs+1))
  
  miss.obs <- which(is.na(obs))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    samps.inc  <- samps.inc[-miss.obs,1,]
  }
  
  crps1 <- crps_sample(obs, samps.inc)
  
  crps1 <- cbind.data.frame(crps1, forecast_ds)   
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) %>%
    mutate(preds = mod1$summary.linear.predictor)
  
  out.list =  list ('ds'=c1.out, 'scores'=scores,  'fixed.eff'=mod1$summary.fixed, 'form'=formula1)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',date.test.in  ,'.rds' )   )

