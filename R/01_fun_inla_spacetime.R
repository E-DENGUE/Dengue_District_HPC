inla_spacetime_mod <- function(vintage_date, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){

  c1 <- d2 %>%
    filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='district') %>%
    arrange(district, date) %>%
    mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop * 100000,
           log_df_rate = log((m_DHF_cases + 1) / pop * 100000),
           log_pop = log(pop / 100000),
           year = lubridate::year(date),
           m_DHF_cases_hold = ifelse(date > vintage_date, NA_real_, m_DHF_cases),
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           max_allowed_lag = ifelse(any(grepl('lag_y', formula1) | grepl('lag1', formula1)), 1, 2),
           horizon = ifelse(date == (vintage_date %m+% months(1)), 1,
                            ifelse(date == (vintage_date %m+% months(2)), 2, 0)
           ),
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (vintage_date %m+% months(2) ) & !is.na(lag2_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(
      districtID2 = districtID,
      districtID3 = districtID,
      districtID4 = districtID,
      t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
      
      time_id1= t , 
      time_id2=t,
      time_id3= t,
      time_id4= t,
      
            urban_dic = as.factor(if_else(Urbanization_Rate>=40,1,0))) %>%
    arrange(date,districtID) %>% #SORT FOR SPACE_TIME
    mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
           timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
           Population_density= scale(Population_density),
           total_rainfall_ab <- scale(total_rainfall_ab),
           ave_temp_ab <- scale(ave_temp_ab),
           max_temp_ab <- scale(max_temp_ab),
           min_ave_temp_ab <- scale(min_ave_temp_ab),
           ave_humid_ab <- scale(ave_humid_ab),
           min_humid_abb <- scale(min_humid_abb),
           max_humid_abb <- scale(max_humid_abb),
           ave_wind_ab <- scale(ave_wind_ab),
           min_wind_ab <- scale(min_wind_ab),
           max_wind_ab <- scale(max_wind_ab),
           avg_daily_temp <- scale(avg_daily_temp),
           avg_max_daily_temp <- scale(avg_max_daily_temp),
           avg_min_daily_temp <- scale(avg_min_daily_temp),
           avg_daily_wind <- scale(avg_daily_wind),
           avg_max_daily_wind <- scale(avg_max_daily_wind),
           avg_daily_humid <- scale(avg_daily_humid),
           avg_max_daily_humid <- scale(avg_max_daily_humid),
           avg_min_daily_humid <- scale(avg_min_daily_humid),
           monthly_cum_ppt <- scale(monthly_cum_ppt),
           avg_min_daily_wind <- scale(avg_min_daily_wind),
           avg_daily_humid <- scale(avg_daily_humid),
           avg_max_daily_humid <- scale(avg_max_daily_humid),
           Poverty_Rate<- scale(Poverty_Rate),
           Inmigration_Rate<- scale(Inmigration_Rate),
           Outmigration_Rate<- scale(Outmigration_Rate),
           NetImmigration_Rate<- scale(NetImmigration_Rate),
           BI_larvae<- scale(BI_larvae),
           HI_larvae<- scale(HI_larvae),
           CI_larvae<- scale(CI_larvae),
           DI<- scale(DI),
           Hygienic_Toilet_Access<- scale(Hygienic_Toilet_Access),
           Monthly_Average_Income_Percapita<- scale(Monthly_Average_Income_Percapita),
           Total_Passenger<- scale(Total_Passenger),
           number_of_outbreak_detection<- scale(number_of_outbreak_detection),
           Hygienic_Toilet_Access<- scale(Hygienic_Toilet_Access),
           Urbanization_Rate=scale(Urbanization_Rate),
           prediomentent=as.factor(prediomentent),
           breeding_site_elimination_campaign=as.factor(breeding_site_elimination_campaign),
           cluster=as.factor(cluster),
           lag1_total_rainfall_ab = scale(lag1_total_rainfall_ab),
           lag2_total_rainfall_ab = scale(lag2_total_rainfall_ab),
           lag2_breeding_site_elimination_campaign = scale(lag2_breeding_site_elimination_campaign),
           lag2_active_spraying = scale(lag2_active_spraying),
           lag2_large_scale_spraying_for_epidemic_response = scale(lag2_large_scale_spraying_for_epidemic_response),
           lag2_communication_or_training = scale(lag2_communication_or_training),
           lag2_number_of_outbreak_detection = scale(lag2_number_of_outbreak_detection),
           lag2_number_of_outbreak_response = scale(lag2_number_of_outbreak_response),
    )
  

  # check_times <- c1 %>% group_by(district) %>% summarize(N=n())
   #check_districts <- c1 %>% group_by(date) %>% summarize(N=n())
   
  form2 <- as.formula (formula1)
  
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
      mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
             horizon = if_else(date== (vintage_date %m+% months(1)),1,
                               if_else(date== (vintage_date %m+% months(2)),2, 0
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

  out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'waic'=mod1$waic,'form'=formula1)
  saveRDS(out.list,paste0('./Results/Results_spacetime/', 'mod',modN,'_',vintage_date  ,'.rds' )   )
  return(out.list)
}
