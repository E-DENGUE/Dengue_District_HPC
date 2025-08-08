inla_spacetime_mod <- function(vintage_date, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){
  
  set.seed(8123)  # Global R seed for reproducibility
  
  
  c1 <- d2 %>%
    filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='fcode') %>%
    arrange(fcode, date) %>%
    mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(fcode) %>%
    mutate(fcode2=fcode,
           Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
           log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
           log_pop_total = log(pop_total / 100000),
           year = lubridate::year(date),
           obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           lag3_y = lag(log_df_rate, 3),
           max_allowed_lag = ifelse(any(grepl('lag_y', formula1) | grepl('lag1', formula1)), 1, 3),
           horizon = ifelse(date == (vintage_date %m+% months(1)), 1,
                            ifelse(date == (vintage_date %m+% months(2)), 2,
                            ifelse(date == (vintage_date %m+% months(3)), 3, 0)
           )),
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop_total/100000,
           #log_offset=log(pop_total/100000)
    ) %>%
    filter(date<= (vintage_date %m+% months(3) ) & !is.na(lag3_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(
      fcodeID2 = fcodeID,
      fcodeID3 = fcodeID,
      fcodeID4 = fcodeID,
      t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
      
      time_id1= t , 
      time_id2=t,
      time_id3= t,
      time_id4= t) %>%
    arrange(date,fcodeID) %>% #SORT FOR SPACE_TIME
    mutate(fcodeIDpad=str_pad(fcodeID, 3, pad = "0", side='left'),
           timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
           Population_density= scale(pop_density),
    )
  
  c1$fcodeID<- as.numeric(c1$fcodeID)

  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = fcodeID2, model = "ar1", 
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
               control.predictor = list(compute=TRUE, link=1),
               control.inla = list(strategy='adaptive', # adaptive gaussian
                                   cmin=0),
               control.fixed = list(mean.intercept=0, 
                                    prec.intercept=0.04, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=8
  )    
  
  mod.family <- mod1$.args$family
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(obs_dengue_cases_hold),1,0)),
           horizon = if_else(date== (vintage_date %m+% months(1)),1,
                             if_else(date== (vintage_date %m+% months(2)),2,
                             if_else(date== (vintage_date %m+% months(3)),3, 0
                             )
           )),
           max_allowed_lag = if_else(grepl('lag_y',formula1 ),1,3)
    )%>%
    filter(horizon <= max_allowed_lag)
  #View(c1 %>% dplyr::select(fcode, date,obs_dengue_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))
  
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)
  
  scores <- scoring_func(score.list)
  

  
  c1.out <- c1 %>%
    dplyr::select(date, fcode, severe_dengue_cases, forecast,horizon ) 
  
  out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'waic'=mod1$waic,'form'=formula1)
  saveRDS(out.list,paste0('./Output/Results/Results_spacetime/', 'mod',modN,'_',vintage_date  ,'.rds' )   )
  return(out.list)
}
