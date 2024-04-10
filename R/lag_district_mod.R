all_district_fwd1 <- function(date.test.in, district.select){

  
  c1a <- d2 %>%
    filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='district') %>%
    arrange(district, date) %>%
    mutate( t = interval(min(date), date) %/% months(1) + 1) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *10000,
           log_df_lag2 = lag(log((m_DHF_cases+0.5) / pop *10000),n=2),
           log_df_lag3 = lag(log((m_DHF_cases+0.5) / pop *10000),n=3),
           log_df_lag4 = lag(log((m_DHF_cases+0.5) / pop *10000),n=4),
           log_df_lag5 = lag(log((m_DHF_cases+0.5) / pop *10000),n=5),
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12)
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (date.test.in[1] %m+% months(1) )  & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
           time_id1= t ) 
  
  all.lags <- c1 %>%
    dplyr::select(district,date,log_df_lag2,log_df_lag3,log_df_lag4,log_df_lag5
    ) %>%
    reshape2::melt(., id.vars=c('district','date')) %>%
    reshape2::dcast(., date ~ district+variable) %>%
    filter(complete.cases(.)) %>%
    mutate_at( vars(-starts_with("date")), ~ if(is.numeric(.)) scale(.) else .) %>% #scale all variables except date
    clean_names()
  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  #nbinomial or poisson
  
  date.test <- '2015-06-01'
  
  c1 <- c1a %>%
    filter(date<='2021-08-01' & district==district.select) %>%
    left_join(all.lags, by='date') %>%
    mutate(offset1= pop/10000,
           m_DHF_cases_hold= ifelse( date>= (date.test), NA_real_,
                                     m_DHF_cases)
    )
  # dplyr::select(-contains(district.select)) #filters out lags from the select district--fix this to work with tidy names
  
  
  all.vars <- paste(names(all.lags)[-1], collapse="+")
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " +lag2_monthly_cum_ppt + lag2_avg_daily_temp + sin12 +cos12 + f(t, model='ar1')"))
  
  offset1 <- c2$offset1
  ptm <- proc.time()
  
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
                                    prec.intercept=1, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=7
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

  scores <- scoring_func(score.list)
  
  #plot(c1$date, mod1$summary.fitted.values$mean)
 # plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))
  
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) 

  out.list =  list ('ds'=c1.out, 'scores'=scores,  'fixed.eff'=mod1$summary.fixed, 'form'=formula1)
  saveRDS(out.list,paste0('./Results/', 'lag_mod',modN,'_',date.test.in  ,district.select,'.rds' )   )
  return(out.list)
}
