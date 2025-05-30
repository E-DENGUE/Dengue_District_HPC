lag_district_pca <- function(vintage_date, district.select, modN){
  
  
  c1a <- d2 %>%
    filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='district') %>%
    arrange(district, date) %>%
    mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *100000,
           log_df_lag2 = lag(log((m_DHF_cases+0.5) / pop *100000),n=2),
           log_df_lag3 = lag(log((m_DHF_cases+0.5) / pop *100000),n=3),
           log_df_lag4 = lag(log((m_DHF_cases+0.5) / pop *100000),n=4),
           log_df_lag5 = lag(log((m_DHF_cases+0.5) / pop *100000),n=5),
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           
    ) %>%
    filter(date<= (vintage_date[1] %m+% months(3) )) %>%  #only keep vintage date and 2 month ahead of that
    ungroup() %>%
    mutate(t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
           time_id1= t ) 
  
  all.lags <- c1a %>%
    dplyr::select(district,date,log_df_lag3,log_df_lag4,log_df_lag5
    ) %>%
    reshape2::melt(., id.vars=c('district','date')) %>%
    reshape2::dcast(., date  ~ district+variable) %>% 
    filter(complete.cases(.)) %>%
    mutate_at( vars(-starts_with("date")), ~ if(is.numeric(.)) scale(.) else .) %>% #scale all variables except date
    clean_names()
  
  # form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  # Assuming all.lag is a list where each element is a data frame or matrix

  
  #nbinomial or poisson
  
 c1b <- c1a %>%
    filter( district==district.select) %>%
    left_join(all.lags, by='date') %>%
    mutate(offset1= pop/100000,
           m_DHF_cases_hold= ifelse( date> (vintage_date), NA_real_,
                                     m_DHF_cases)
    )
  # dplyr::select(-contains(district.select)) #filters out lags from the select district--fix this to work with tidy names
  
  ##Y-AWARE PCA
      df2 <- c1b %>% filter( !is.na(an_minh_log_df_lag4 ))
      x <- df2[,names(all.lags)]
      Y <- df2$m_DHF_cases_hold
      y.aware.scale<- apply(x[,-1], 2, function(x1){
        x1 <- as.vector(x1)
        log.y.pre.scale<- scale(log(Y+0.5))
        log.y.pre.scale<- as.vector(log.y.pre.scale)
        reg<-lm(log.y.pre.scale~x1)
        slope<- reg$coefficients[2]
        x.scale<-x1*slope - mean(x1*slope)
        return(x.scale)
      })
      pca1<- prcomp(y.aware.scale, center = FALSE,scale. = FALSE)
      # plot(pca1$sdev)
      n.pcs.keep<-10
      pcs<-pca1$x
      pcs<- as.data.frame(apply(pcs,2, scale)) #SCALE THE PCS prior to regression!
      names(pcs) <- paste0('PC', 1:ncol(pcs))
    pc.df <- cbind.data.frame('date'=df2$date, pcs[,1:n.pcs.keep])
  
    c1 <- c1b %>%
      left_join(pc.df, by='date') 
      
        
  all.vars <- paste(names(pc.df)[-1], collapse="+")
  
  mod.select = mods[as.numeric(modN)]
  
 if (mod.select == 'PC_lags_weather') {
  form2 <- as.formula(paste0("m_DHF_cases_hold ~ ", all.vars, " + lag2_monthly_cum_ppt + lag2_avg_daily_temp + sin12 + cos12 + f(t, model='ar1')"))
} else if (mod.select == 'PC_lags') {
  form2 <- as.formula(paste0("m_DHF_cases_hold ~ ", all.vars, " + sin12 + cos12 + f(t, model='ar1')"))
} else if (mod.select == 'PC_weather') {
  form2 <- as.formula(paste0("m_DHF_cases_hold ~ ", all.vars, " + lag2_monthly_cum_ppt + lag2_avg_daily_temp + sin12 + cos12 + f(t, model='ar1')"))
} else if (mod.select == 'PC_without_seasonality') {
  form2 <- as.formula(paste0("m_DHF_cases_hold ~ ", all.vars, " + lag2_monthly_cum_ppt + lag2_avg_daily_temp + f(t, model='ar1')"))
} else if (mod.select == 'PC_without_seasonality_and_weather') {
  form2 <- as.formula(paste0("m_DHF_cases_hold ~ ", all.vars, " + f(t, model='ar1')"))
}

  
  offset1 <- c1$offset1
  #ptm <- proc.time()
  
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
               num.threads=7
  )    
  
  
  mod.family <- mod1$.args$family
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
           horizon = if_else(date== (vintage_date %m+% months(1)),1,
                             if_else(date== (vintage_date %m+% months(2)),2,
                                     if_else(date== (vintage_date %m+% months(3)),3, 0
                             )
           )),
           max_allowed_lag = 3
    )%>%
    filter(horizon <= max_allowed_lag) #get rid of lag2 if lag1 is included as covariate
  #View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))
  
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)
  
  scores <- scoring_func(score.list)
  
  #plot(c1$date, mod1$summary.fitted.values$mean)
  # plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))
  
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) 
  
  out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'form'==as.character(form2))
  
  saveRDS(out.list,paste0('./Results/Results_pca_2022/', mod.select,'_',district.select,'_',vintage_date  ,'.rds' )   )
  return(out.list)
}

##

